package com.drillinginfo.dca;

import java.io.PrintStream;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class DCAdriver
{
  private String _wellName;
  private String _flowName;
  private String _ioUnits;
  private String _modelFailure = null;
  private long[] _dates;
  private double[] _production;
  private double[] _modeled;
  private int _maxMonth;
  private int _numTrials = 1000;
  private int _maxProducingMonths = 600;
  private int _hindcastMonth = 6;
  private List<DCAmodel> _bestModel;
  private List<DCAmodel> _dcaModels;
  private boolean _useProduction = true;
  private boolean _useCumulative = false;
  private boolean _testSegmentation = false;
  private boolean _outlierAnalysis = false;
  private double _segmentThreshold = 3.0D;
  private double _outlierThreshold = 3.0D;
  private String _regressionScale = "Linear";
  private String _tailDecline = "Natural";
  private boolean _searchDecline = false;
  private boolean _nominalDecline = true;
  private double _minDecline = 0.05D;
  private double _minProduction = 1000.0D;
  private double _transitionSlope = 0.05D;
  private double _transitionMonth = 0.0D;
  private double[] _arpsLoExtremes = { 1.0D, 1.0E-6D, 1.0E-4D, 6.0D };
  private double[] _arpsHiExtremes = { 120000.0D, 0.8D, 2.0D, 1200.0D };
  private double[] _sepdLoExtremes = { 1.0D, 0.15D, 0.01D };
  private double[] _sepdHiExtremes = { 120000.0D, 20.0D, 5.0D };
  private double[] _duongLoExtremes = { 1.0D, 0.5D, 1.0D };
  private double[] _duongHiExtremes = { 120000.0D, 2.5D, 1.3D };
  private double[] _lgmLoExtremes = { 1.0D, 1000.0D, 0.01D };
  private double[] _lgmHiExtremes = { 1000.0D, 1.0E8D, 1.0D };
  private static int _nProductionMin = 5;
  private static final long SPAN = 1000L;
  

  //Default Constructor
  public DCAdriver() {}
  
  @Deprecated
  public DCAdriver(double[] production)
  {
    this(production, "m3");
  }
  
  public DCAdriver(double[] production, String ioUnits)
    throws IllegalArgumentException
  {
    if ((!ioUnits.equals("bbl")) && (!ioUnits.equals("Mcf")))
    {
      String message = "Input units must be one of bbl, Mcf, or m3.";
      throw new IllegalArgumentException(message);
    }
    setProduction(production);
  }
  
  @Deprecated
  public DCAdriver(long[] dates, double[] production)
  {
    this(dates, production, "m3");
  }
  
  public DCAdriver(Date[] dates, double[] production, String ioUnits)
    throws IllegalArgumentException
  {
    if (dates.length != production.length)
    {
      String message = "Lengths of the date and production arrays are different.";
      throw new IllegalArgumentException(message);
    }
    if ((!ioUnits.equals("bbl")) && (!ioUnits.equals("Mcf")))
    {
      String message = "Input units must be one of bbl, Mcf, or m3.";
      throw new IllegalArgumentException(message);
    }
    long[] datesLong = new long[dates.length];
    for (int i = 0; i < dates.length; i++) {
      datesLong[i] = (dates[i].getTime() * 1000L);
    }
    createDriver(datesLong, production, ioUnits);
  }
  
  public DCAdriver(long[] dates, double[] production, String ioUnits)
    throws IllegalArgumentException
  {
    System.out.println("HERE in DCAdriver constructor");
    if (dates.length != production.length)
    {
      String message = "Lengths of the date and production arrays are different.";
      throw new IllegalArgumentException(message);
    }
    if ((!ioUnits.equals("bbl")) && (!ioUnits.equals("Mcf")))
    {
      String message = "Input units must be one of bbl, Mcf, or m3.";
      throw new IllegalArgumentException(message);
    }
    createDriver(dates, production, ioUnits);
  }
  
  public DCAdriver(int[] dates, double[] production, String ioUnits)
  {
    long[] longDates = new long[dates.length];
    for (int i = 0; i < dates.length; i++)
    {
      Integer integer = new Integer(dates[i]);
      longDates[i] = (integer.longValue() * 1000L);
    }
    createDriver(longDates, production, ioUnits);
  }
  
  public DCAdriver(double[] dates, double[] production, String ioUnits)
  {
    long[] longDates = new long[dates.length];
    for (int i = 0; i < dates.length; i++)
    {
      Double d = new Double(dates[i]);
      longDates[i] = (d.longValue() * 1000L);
    }
    createDriver(longDates, production, ioUnits);
  }
  
  public DCAdriver(float[] dates, double[] production, String ioUnits)
  {
    long[] longDates = new long[dates.length];
    for (int i = 0; i < dates.length; i++)
    {
      Float f = new Float(dates[i]);
      longDates[i] = (f.longValue() * 1000L);
    }
    createDriver(longDates, production, ioUnits);
  }


/*-------------------------------------------------------------------------------------------------------*/\

//Called in DCAdriver constructor, saves values to variables?
  private void createDriver(long[] dates, double[] production, String ioUnits)
  {
    this._ioUnits = ioUnits;
    
    this._dates = DCAUtil.getMonthLongValues(dates[0], dates[(dates.length - 1)]);
    this._production = DCAUtil.getFullProduction(this._dates, dates, production);
    if (this._dates.length != this._production.length)
    {
      long[] temp = new long[this._production.length];
      for (int j = 0; j < this._production.length; j++) {
        temp[j] = this._dates[j];
      }
      this._dates = temp;
    }
    setProduction(this._production);
  }
  
  public void setMinimizeRate(boolean state)
  {
    this._useProduction = state;
  }
  
  public boolean getMinimizeRate()
  {
    return this._useProduction;
  }
  
  public void setMinimizeCumulative(boolean state)
  {
    this._useCumulative = state;
  }
  
  public boolean getMinimizeCumulative()
  {
    return this._useCumulative;
  }
  
  public void setMonths(long[] dates)
  {
    this._dates = dates;
  }
  
  public long[] getMonths()
  {
    return this._dates;
  }
  
  public void setWellName(String name)
  {
    this._wellName = name;
  }
  
  public String getWellName()
  {
    return this._wellName;
  }
  
  public void setFlowName(String name)
  {
    this._flowName = name;
  }
  
  public String getFlowName()
  {
    return this._flowName;
  }
  
  private void setProduction(double[] production)
  {
    this._production = production;
    
    int nmonths = 0;
    for (int i = 0; i < this._production.length; i++)
    {
      if (this._production[i] > 0.0D) {
        nmonths++;
      }
      if (nmonths == 20)
      {
        nmonths = i; break;
      }
    }
    this._maxMonth = 1;
    double pmax = this._production[0];
    this._modeled = new double[this._production.length];
    this._modeled[0] = this._production[0];
    for (int i = 1; i < Math.min(this._production.length, nmonths); i++)
    {
      if (this._production[i] > pmax)
      {
        pmax = this._production[i];this._maxMonth = (i + 1);
      }
      this._modeled[i] = this._production[i];
    }
  }
  
  public void setIOunits(String units)
  {
    this._ioUnits = units;
  }
  
  public String getIOunits()
  {
    return this._ioUnits;
  }
  
  public double[] getProduction()
  {
    return this._production;
  }
  
  public double getCumulativeProduction()
  {
    double[] cums = getCumulativeProductionStream();
    return cums[(cums.length - 1)];
  }
  
  public double[] getCumulativeProductionStream()
  {
    double[] cums = new double[this._production.length];
    if (!Double.isNaN(this._production[0])) {
      cums[0] = this._production[0];
    }
    int i = 1;
    for (int im1 = 0; i < this._production.length; im1++)
    {
      cums[i] = cums[im1];
      if (!Double.isNaN(this._production[i])) {
        cums[i] += this._production[i];
      }
      i++;
    }
    return cums;
  }
  
  public double[] getModeledProduction()
  {
    return this._modeled;
  }
  
  public boolean[] getOutliers()
  {
    if (this._bestModel == null) {
      return null;
    }
    boolean[] outliers = new boolean[this._production.length];
    int n = 0;
    for (int im = 0; im < this._bestModel.size(); im++)
    {
      boolean[] os = ((DCAmodel)this._bestModel.get(im)).getOutliers();
      for (int i = 0; i < os.length; i++) {
        outliers[(n++)] = os[i];
      }
    }
    return outliers;
  }
  
  public double[] getPrediction()
  {
    double q = 0.0D;
    double[] prediction = new double[this._production.length];
    for (int i = 0; i < prediction.length; i++)
    {
      prediction[i] = getPredicted(i + 1, q);
      q = prediction[i];
    }
    if (this._ioUnits.equals("bbl")) {
      for (int i = 0; i < prediction.length; i++) {
        prediction[i] /= DCAUtil._bbl2m3;
      }
    } else if (this._ioUnits.equals("Mcf")) {
      for (int i = 0; i < prediction.length; i++) {
        prediction[i] /= DCAUtil._Mcf2m3;
      }
    }
    return prediction;
  }
  
  public double[] getCumulativePrediction()
  {
    double[] prediction = getPrediction();
    
    double[] cums = new double[prediction.length];
    cums[0] = prediction[0];
    int i = 1;
    for (int im1 = 0; i < prediction.length; im1++)
    {
      cums[im1] += prediction[i];i++;
    }
    return cums;
  }
  
  public double getPredicted(double t, double q)
  {
    if (this._bestModel == null) {
      return NaN.0D;
    }
    if (this._bestModel.size() == 1)
    {
      DCAmodel model = (DCAmodel)this._bestModel.get(0);
      if (model == null) {
        return NaN.0D;
      }
      return model.model(t);
    }
    int t1 = 0;int t2 = 0;
    for (int i = 0; i < this._bestModel.size(); i++)
    {
      int l = ((DCAmodel)this._bestModel.get(i)).getProduction().length;
      t2 += l;
      if (i == this._bestModel.size() - 1) {
        t2++;
      }
      if (t <= t2) {
        return ((DCAmodel)this._bestModel.get(i)).model(t - t1);
      }
      if (i + 1 >= this._bestModel.size()) {
        return NaN.0D;
      }
      ((DCAmodel)this._bestModel.get(i + 1)).setQo(q);
      t1 = t2;
    }
    return NaN.0D;
  }
  
  public void setTrialCount(int numTrials)
  {
    if (numTrials < 100)
    {
      String message = "The number of trials must be at least 100.";
      throw new IllegalArgumentException(message);
    }
    this._numTrials = numTrials;
  }
  
  public double[][] getTrialParameters()
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return (double[][])null;
    }
    return dca.getTrialParameters();
  }
  
  public double getEUR()
  {
    double cumulative = getCumulativeProduction();
    double remaining = getEstimatedReserves();
    if (Double.isNaN(remaining)) {
      return remaining;
    }
    return cumulative + remaining;
  }
  
  public double getEURrate()
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return NaN.0D;
    }
    return dca.getEURrate();
  }
  
  public Date getEURdate()
  {
    int imonth = getEURmonth();
    if (imonth < 0) {
      return new Date(this._dates[(this._dates.length - 1)]);
    }
    return new Date(this._dates[0] + imonth * 2592000000L);
  }
  
  public int getEURmonth()
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return -1;
    }
    int imonth = dca.getEURmonth();
    if (imonth < 0) {
      return imonth;
    }
    for (int i = 0; i < this._bestModel.size() - 1; i++)
    {
      DCAmodel model = (DCAmodel)this._bestModel.get(i);
      imonth += model.getProduction().length;
    }
    return imonth;
  }
  
  public double[] getEURs()
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return null;
    }
    double[] eurs = dca.getEURs();
    if (eurs != null)
    {
      double cumulative = getCumulativeProduction();
      for (int i = 0; i < eurs.length; i++) {
        eurs[i] += cumulative;
      }
    }
    return eurs;
  }
  
  public double[] getReserves()
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return null;
    }
    return dca.getReserves();
  }
  
  public double getEstimatedReserves()
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return NaN.0D;
    }
    return dca.getEstimatedReserves();
  }
  
  public double getReserves(double p)
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return NaN.0D;
    }
    return dca.getReserves(p);
  }
  
  public void setSegmentation(boolean state, double threshold)
    throws IllegalArgumentException
  {
    if ((state) && (threshold <= 0.0D))
    {
      String message = "Threshold must be greater than 0 when requesting segmentation.";
      throw new IllegalArgumentException(message);
    }
    this._testSegmentation = state;
    this._segmentThreshold = threshold;
  }
  
  public boolean getSegmentation()
  {
    return this._testSegmentation;
  }
  
  public double getSegmentThreshold()
  {
    return this._segmentThreshold;
  }
  
  public void model()
    throws IllegalArgumentException, IllegalStateException
  {
    String errors = errorTest(); /*line 886 Checks if this._production is null/*/
    if (!errors.isEmpty())
    {
      System.out.println(errors);
      return;
    }
    try
    {
      this._modelFailure = null;

      validStream(this._production); /* Check if has more than 5 good production values and if more than 75 percent of values are negative*/
    }
    catch (IllegalStateException exp)
    {
      this._modelFailure = exp.getMessage();
      throw exp;
    }
    DCAmodel bestModel = null;
    try
    {
      bestModel = bestFit(this._production, false, this._transitionMonth); /* */
    }
    catch (IllegalStateException exp)
    {
      throw exp;
    }
    this._bestModel = new ArrayList();this._bestModel.add(bestModel);
    this._modeled = bestModel.getModeledProduction();
    if (this._testSegmentation)
    {
      if (!modelSegments())
      {
        while (((DCAmodel)this._bestModel.get(0)).outliers()) {}
        this._modeled = ((DCAmodel)this._bestModel.get(0)).getModeledProduction();
      }
    }
    else {
      while (bestModel.outliers()) {}
    }
  }
  
  public void validStream(double[] production)
    throws IllegalStateException
  {
    int nvalues = 0;
    for (int i = 0; i < this._production.length; i++) {
      if ((!Double.isNaN(this._production[i])) && (this._production[i] > 0.0D)) { /* Only counts values if >0 and not NaN*/
        nvalues++;
      }
    }
    if (nvalues < _nProductionMin) /* minimum 5 points*/
    {
      String exception = this._wellName + " does not have sufficient number of valid production values.";
      throw new IllegalStateException(exception);
    }
    int nbad = 0;
    for (int i = 0; i < this._production.length; i++) {
      if ((Double.isNaN(this._production[i])) || (this._production[i] <= 0.0D)) {
        nbad++;
      }
    }
    if (nbad / this._production.length > 0.75D)
    {
      String exception = this._wellName + " contains less than 25% positive production values.";
      throw new IllegalStateException(exception);
    }
  }
  
  public String getModelFailure()
  {
    return this._modelFailure;
  }
  
  void prePostModels()
  {
    DCAmodel bestModel = bestFit(this._production, false, this._transitionMonth);
    this._bestModel = new ArrayList();this._bestModel.add(bestModel);
    this._modeled = bestModel.getModeledProduction();
    if (!modelSegments())
    {
      while (((DCAmodel)this._bestModel.get(0)).outliers()) {}
      this._modeled = ((DCAmodel)this._bestModel.get(0)).getModeledProduction();
    }
  }
  
  void mergePrePostModels(DCAdriver preDriver, DCAdriver postDriver)
  {
    List<DCAmodel> preModels = preDriver.getModel();
    List<DCAmodel> postModels = postDriver.getModel();
    this._bestModel = new ArrayList();
    for (int i = 0; i < preModels.size(); i++) {
      this._bestModel.add(((DCAmodel)preModels.get(i)).copy());
    }
    for (int i = 0; i < postModels.size(); i++) {
      this._bestModel.add(((DCAmodel)postModels.get(i)).copy());
    }
    double[] preModeled = preDriver.getModeledProduction();
    double[] postModeled = postDriver.getModeledProduction();
    this._modeled = new double[preModeled.length + postModeled.length];
    int j = 0;
    for (int i = 0; i < preModeled.length; i++) {
      this._modeled[(j++)] = preModeled[i];
    }
    for (int i = 0; i < postModeled.length; i++) {
      this._modeled[(j++)] = postModeled[i];
    }
  }
  
  private boolean modelSegments()
  {
    boolean found = false;
    int maxMonth = this._maxMonth;
    String temp = this._tailDecline;
    this._tailDecline = "Natural";
    
    this._modeled = ((DCAmodel)this._bestModel.get(0)).getModeledProduction();
    DCAmodel current = (DCAmodel)this._bestModel.get(0);
    if (!current.getTailDecline().equals("Natural"))
    {
      current = ((DCAmodel)this._bestModel.get(0)).copy();
      current.setTailDecline("Natural");
      current.autoFit(true);
    }
    double[] predicted = current.getPrediction();
    double[] residuals = new double[this._modeled.length];
    for (int i = 0; i < this._modeled.length; i++) {
      if (this._modeled[i] > 0.0D) {
        residuals[i] = ((this._modeled[i] - predicted[i]) / predicted[i]);
      }
    }
    double sum = 0.0D;double sum2 = 0.0D;double avg = 0.0D;double std = 0.0D;double lastAvg = 0.0D;double lastStd = 0.0D;
    int n = 0;int nsegments = 0;
    int[] indices = new int[100];
    for (int i = 0; i < this._modeled.length; i++) {
      if (this._modeled[i] > 0.0D)
      {
        n++;sum += residuals[i];avg = sum / n;
        sum2 += residuals[i] * residuals[i];
        std = Math.sqrt((sum2 - sum * sum / n) / n);
        
        double zscore = (residuals[i] - lastAvg) / lastStd;
        boolean sufficient = true;
        int remaining = 0;
        for (int j = i; j < this._modeled.length; j++) {
          if ((!Double.isNaN(this._modeled[j])) && (this._modeled[j] > 0.0D)) {
            remaining++;
          }
        }
        if (((nsegments > 0) && (i - indices[(nsegments - 1)] < _nProductionMin)) || (remaining < _nProductionMin) || (n - 1 < _nProductionMin)) {
          sufficient = false;
        }
        if ((zscore > this._segmentThreshold) && (i > maxMonth) && (sufficient))
        {
          if (nsegments == 0)
          {
            double[] production = new double[i];
            for (int j = 0; j < production.length; j++) {
              production[j] = this._modeled[j];
            }
            DCAmodel bestModel = bestFit(production, this._outlierAnalysis, this._transitionMonth);
            this._bestModel.clear();
            this._bestModel.add(bestModel);
            
            double[] modeled = bestModel.getModeledProduction();
            for (int j = 0; j < modeled.length; j++) {
              this._modeled[j] = modeled[j];
            }
          }
          else
          {
            int is = nsegments - 1;
            double[] production = new double[i - indices[is]];
            int j = 0;
            for (int k = indices[is]; j < production.length; k++)
            {
              production[j] = this._modeled[k];j++;
            }
            if (!validSampling(production)) {
              continue;
            }
            DCAmodel dcaSegment = getLastSegmentModel();
            dcaSegment.setProduction(production, this._ioUnits);
            dcaSegment.setTailDecline("Natural");
            maxMonth = i + dcaSegment._maxMonth - 1;
            dcaSegment.autoFit(false);
            while (dcaSegment.outliers()) {}
            double[] modeled = dcaSegment.getModeledProduction();
            int j = 0;
            for (int k = indices[is]; j < modeled.length; k++)
            {
              this._modeled[k] = modeled[j];j++;
            }
          }
          double[] production = new double[this._modeled.length - i];
          int j = 0;
          for (int k = i; j < production.length; k++)
          {
            production[j] = this._modeled[k];j++;
          }
          double sumMonths = 0.0D;
          for (int j = 0; j <= nsegments; j++) {
            sumMonths += ((DCAmodel)this._bestModel.get(j)).getProduction().length;
          }
          int is;
          try
          {
            DCAmodel nextModel = bestFit(production, false, this._transitionMonth - sumMonths);
            
            predicted = nextModel.getPrediction();
            double[] modeled = nextModel.getModeledProduction();
            int j = i;
            for (int k = 0; k < predicted.length; k++)
            {
              this._modeled[j] = modeled[k];
              residuals[j] = ((modeled[k] - predicted[k]) / predicted[k]);j++;
            }
            this._bestModel.add(nextModel);
            indices[nsegments] = i;
            nsegments++;
            found = true;
          }
          catch (IllegalStateException exp)
          {
            System.out.println("Segment model failed");
            
            is = nsegments - 1;
            production = new double[this._modeled.length - indices[is]];
            int j = 0;
            for (int k = indices[is]; k < this._modeled.length; k++)
            {
              production[j] = this._modeled[k];j++;
            }
            if (validSampling(production)) {
              break label984;
            }
          }
          continue;
          label984:
          DCAmodel dcaSegment = getLastSegmentModel();
          dcaSegment.setProduction(production, this._ioUnits);
          dcaSegment.setTailDecline("Natural");
          maxMonth = i + dcaSegment._maxMonth - 1;
          dcaSegment.autoFit(false);
          while (dcaSegment.outliers()) {}
          double[] modeled = dcaSegment.getModeledProduction();
          int j = 0;
          for (int k = indices[is]; j < modeled.length; k++)
          {
            this._modeled[k] = modeled[j];j++;
          }
        }
        else
        {
          lastAvg = avg;lastStd = std;
        }
      }
    }
    if ((this._bestModel.size() > 1) && (nsegments > 0))
    {
      DCAmodel dcaSegment = getLastSegmentModel();
      if (!temp.equals("Natural"))
      {
        dcaSegment.setTailDecline(temp);
        dcaSegment.autoFit(false);
      }
      while (dcaSegment.outliers()) {}
      double[] modeled = dcaSegment.getModeledProduction();
      int j = indices[(nsegments - 1)];
      for (int k = 0; k < modeled.length; k++)
      {
        this._modeled[j] = modeled[k];j++;
      }
      this._tailDecline = temp;
    }
    else
    {
      if (getLastSegmentModel() != null)
      {
        this._tailDecline = temp;
        return found;
      }
      this._modelFailure = null;
      this._tailDecline = "Natural";
      DCAmodel bestModel = bestFit(this._modeled, this._outlierAnalysis, this._transitionMonth);
      this._bestModel.clear();
      this._bestModel.add(bestModel);
      this._tailDecline = temp;
    }
    return found;
  }
  
  private boolean validSampling(double[] production)
  {
    int n = 0;
    for (int i = 0; i < production.length; i++) {
      if (production[i] > 0.0D) {
        n++;
      }
    }
    return n > 4;
  }
  
  private DCAmodel bestFit(double[] production, boolean outliers, double transitionMonth)
    throws IllegalStateException
  {
    if (this._dcaModels == null) {
      setModelTypes(); /* selects correct model type and creatse class var for that model type, then it sets low and high levels from instantaited class var.*/
    } else {
      resetModelTypes();
    }
    DCAmodel bestModel = null;double sseMin = Double.MAX_VALUE;
    Iterator<DCAmodel> iterator = this._dcaModels.iterator();
    while (iterator.hasNext())
    {
      DCAmodel dcaSegment = (DCAmodel)iterator.next();
      dcaSegment.setProduction(production, this._ioUnits);
      dcaSegment.setMinimizeRate(this._useProduction);
      dcaSegment.setMinimizeCumulative(this._useCumulative);
      dcaSegment.setSearchDecline(this._searchDecline);
      dcaSegment.setNominalDecline(this._nominalDecline);
      dcaSegment.setTailDecline(this._tailDecline);
      dcaSegment.setMinimumDecline(this._minDecline);
      dcaSegment.setTransitionSlope(this._transitionSlope);
      dcaSegment.setTransitionMonth(transitionMonth);
      try
      {
        dcaSegment.autoFit(true);
      }
      catch (IllegalStateException exp)
      {
        this._modelFailure = (this._wellName + ": " + exp.getMessage());
        throw exp;
      }
      while ((outliers) && (dcaSegment.outliers())) {}
      if (dcaSegment._sseMin < sseMin)
      {
        sseMin = dcaSegment._sseMin;bestModel = dcaSegment;
      }
    }
    return bestModel;
  }
  
  public List<DCAmodel> getModel()
  {
    return this._bestModel;
  }
  
  public DCAmodel getLastSegmentModel()
  {
    if (this._bestModel != null) {
      return (DCAmodel)this._bestModel.get(this._bestModel.size() - 1);
    }
    return null;
  }
  
  public String getLastSegmentModelType()
  {
    DCAmodel lastModel = getLastSegmentModel();
    if (lastModel == null) {
      return null;
    }
    return lastModel.getType();
  }
  
  public double getTransitionMonth()
  {
    double month = 0.0D;
    if (this._bestModel != null)
    {
      for (int i = 0; i < this._bestModel.size() - 1; i++) {
        month += ((DCAmodel)this._bestModel.get(i)).getProduction().length;
      }
      month += getLastSegmentModel().getDeclineMonth();
    }
    return month;
  }
  
  public String errorTest()
  {
    String errors = "";
    if (this._production == null) {
      errors = errors + "No production stream available for analysis.\n";
    }
    return errors;
  }
  
  public void setModelTypes()
  {
    this._dcaModels = new ArrayList();
    DCAmodel dcaArps = new DCAarps();this._dcaModels.add(dcaArps);
    DCAmodel dcaSEPD = new DCAsepd();this._dcaModels.add(dcaSEPD);
    DCAmodel dcaPowr = new DCApowerLaw();this._dcaModels.add(dcaPowr);
    DCAmodel dcaDuon = new DCAduong();this._dcaModels.add(dcaDuon);
    DCAmodel dcaLogs = new DCAlgm();this._dcaModels.add(dcaLogs);
    
    dcaArps.setLoExtremes(this._arpsLoExtremes);
    dcaArps.setHiExtremes(this._arpsHiExtremes);
    dcaSEPD.setLoExtremes(this._sepdLoExtremes);
    dcaSEPD.setHiExtremes(this._sepdHiExtremes);
    dcaDuon.setLoExtremes(this._duongLoExtremes);
    dcaDuon.setHiExtremes(this._duongHiExtremes);
    dcaLogs.setLoExtremes(this._lgmLoExtremes);
    dcaLogs.setHiExtremes(this._lgmHiExtremes);
  }
  
  public void setModelExtremes(String type, double[] lo, double[] hi)
  {
    if (type.equals("Arps"))
    {
      this._arpsLoExtremes = ((double[])lo.clone());this._arpsHiExtremes = ((double[])hi.clone());
    }
    if (type.equals("SEPD"))
    {
      this._sepdLoExtremes = ((double[])lo.clone());this._sepdHiExtremes = ((double[])hi.clone());
    }
    if (type.equals("Duong"))
    {
      this._duongLoExtremes = ((double[])lo.clone());this._duongHiExtremes = ((double[])hi.clone());
    }
    if (type.equals("Logistic Growth"))
    {
      this._lgmLoExtremes = ((double[])lo.clone());this._lgmHiExtremes = ((double[])hi.clone());
    }
  }
  
  public List<DCAmodel> getModelTypes()
  {
    return this._dcaModels;
  }
  
  public void setModelTypes(ArrayList<String> types)
  {
    this._dcaModels = new ArrayList();
    if (types.contains("Arps")) {
      this._dcaModels.add(new DCAarps());
    }
    if (types.contains("SEPD")) {
      this._dcaModels.add(new DCAsepd());
    }
    if (types.contains("Duong")) {
      this._dcaModels.add(new DCAduong());
    }
    if (types.contains("Power Law")) {
      this._dcaModels.add(new DCApowerLaw());
    }
    if (types.contains("Logistic Growth")) {
      this._dcaModels.add(new DCAlgm());
    }
  }
  
  private void resetModelTypes()
  {
    if (this._dcaModels == null) {
      return;
    }
    List<DCAmodel> models = new ArrayList();
    for (int i = 0; i < this._dcaModels.size(); i++)
    {
      DCAmodel dca = (DCAmodel)this._dcaModels.get(i);
      if ((dca instanceof DCAarps))
      {
        DCAmodel dm = new DCAarps();
        dm.setLoExtremes(this._arpsLoExtremes);
        dm.setHiExtremes(this._arpsHiExtremes);
        models.add(dm);
      }
      else if ((dca instanceof DCAsepd))
      {
        DCAmodel dm = new DCAsepd();
        dm.setLoExtremes(this._sepdLoExtremes);
        dm.setHiExtremes(this._sepdHiExtremes);
        models.add(dm);
      }
      else if ((dca instanceof DCAduong))
      {
        DCAmodel dm = new DCAduong();
        dm.setLoExtremes(this._duongLoExtremes);
        dm.setHiExtremes(this._duongHiExtremes);
        models.add(dm);
      }
      else if ((dca instanceof DCApowerLaw))
      {
        models.add(new DCApowerLaw());
      }
      else if ((dca instanceof DCAlgm))
      {
        DCAmodel dm = new DCAlgm();
        dm.setLoExtremes(this._lgmLoExtremes);
        dm.setHiExtremes(this._lgmHiExtremes);
        models.add(dm);
      }
      else if ((dca instanceof DCAhyp2exp))
      {
        models.add(new DCAhyp2exp());
      }
    }
    this._dcaModels = models;
    setRegressionScaling(this._regressionScale);
    setOutlierAnalysis(this._outlierAnalysis, this._outlierThreshold);
  }
  
  public void setRegressionScaling(String type)
  {
    this._regressionScale = type;
    if (this._dcaModels == null) {
      return;
    }
    for (int i = 0; i < this._dcaModels.size(); i++) {
      ((DCAmodel)this._dcaModels.get(i)).setRegressionScaling(type);
    }
  }
  
  public String getRegressionScaling()
  {
    return this._regressionScale;
  }
  
  public void setOutlierAnalysis(boolean state, double threshold)
  {
    this._outlierAnalysis = state;
    this._outlierThreshold = threshold;
    if (this._dcaModels == null) {
      return;
    }
    for (int i = 0; i < this._dcaModels.size(); i++)
    {
      DCAmodel dca = (DCAmodel)this._dcaModels.get(i);
      dca.setOutlierAnalysis(this._outlierAnalysis, this._outlierThreshold);
    }
  }
  
  public boolean getOutlierAnalysis()
  {
    return this._outlierAnalysis;
  }
  
  public double getOutlierThreshold()
  {
    return this._outlierThreshold;
  }
  
  public void setHindcastMonth(int month)
  {
    this._hindcastMonth = month;
  }
  
  public int getHindcastMonth()
  {
    return this._hindcastMonth;
  }
  
  public void setMinimumProduction(double minimum)
  {
    this._minProduction = minimum;
    DCAmodel dca = getLastSegmentModel();
    dca.setMinimumProduction(minimum);
  }
  
  public void setMaximumProducingMonths(int maximum)
  {
    this._maxProducingMonths = maximum;
    DCAmodel dca = getLastSegmentModel();
    dca.setMaximumProducingMonths(this._maxProducingMonths);
  }
  
  public void setForecastYears(double nyears)
  {
    DCAmodel dca = getLastSegmentModel();
    dca.setForecastYears(nyears);
  }
  
  public void setForecastReference(String reference)
  {
    DCAmodel dca = getLastSegmentModel();
    dca.setForecastReference(reference);
  }
  
  public void simulation()
    throws IllegalArgumentException, IllegalStateException
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null)
    {
      String message = "Model must be generated using model() method prior to simulation.";
      throw new IllegalArgumentException(message);
    }
    dca.setTrialCount(this._numTrials);
    if (dca.getPrediction() == null) {
      return;
    }
    try
    {
      dca.computeBayesianTrials();
    }
    catch (IllegalStateException exp)
    {
      throw exp;
    }
  }
  
  public void hindcast()
    throws IllegalArgumentException
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null)
    {
      String message = "Model must be generated using model() method prior to hindcasting.";
      throw new IllegalArgumentException(message);
    }
    dca.setHindcastMonth(this._hindcastMonth);
    dca.hindcast();
  }
  
  public double[][] getHindcastResults()
    throws IllegalArgumentException
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null) {
      return (double[][])null;
    }
    return dca.getHindcastResults();
  }
  
  public void forecast()
    throws IllegalArgumentException, IllegalStateException
  {
    DCAmodel dca = getLastSegmentModel();
    if (dca == null)
    {
      String message = "Model must be generated using model() method prior to forecasting.";
      throw new IllegalArgumentException(message);
    }
    dca.setTailDecline(this._tailDecline);
    dca.setMinimumDecline(this._minDecline);
    dca.setTrialCount(this._numTrials);
    if (dca.getPrediction() == null) {
      return;
    }
    try
    {
      dca.bayesian();
    }
    catch (IllegalStateException exp)
    {
      throw exp;
    }
  }
  
  public void setSearchDecline(boolean search)
  {
    this._searchDecline = search;
  }
  
  public boolean getSearchDecline()
  {
    return this._searchDecline;
  }
  
  public void setNominalDecline(boolean decline)
  {
    this._nominalDecline = decline;
  }
  
  public boolean getNominalDecline()
  {
    return this._nominalDecline;
  }
  
  public void setTailDecline(String decline)
    throws IllegalArgumentException
  {
    if ((!decline.equals("Natural")) && 
      (!decline.equals("Minimum")) && 
      (!decline.equals("Exponential")))
    {
      String message = "Decline tail must be one of Natural, Minimum, or Exponential.";
      throw new IllegalArgumentException(message);
    }
    this._tailDecline = decline;
  }
  
  public String getTailDecline()
  {
    return this._tailDecline;
  }
  
  public void setTransitionSlope(double slope)
  {
    this._transitionSlope = slope;
  }
  
  public double getTransitionSlope()
  {
    return this._transitionSlope;
  }
  
  public void setTransitionMonth(double month)
  {
    this._transitionMonth = month;
  }
  
  public void setMinimumDecline(double decline)
    throws IllegalArgumentException
  {
    if ((decline <= 0.0D) || (decline >= 1.0D))
    {
      String message = "Decline rate must be between 0 and 1 exclusively";
      throw new IllegalArgumentException(message);
    }
    this._minDecline = decline;
  }
  
  public double getMinimumDecline()
  {
    return this._minDecline;
  }
  
  public double[] getModelForecast()
  {
    DCAmodel dca = getLastSegmentModel();
    return dca.getModelForecast();
  }
  
  public double[] getPCurve(double p)
  {
    DCAmodel dca = getLastSegmentModel();
    return dca.getPCurve(p);
  }
  
  public String getReport()
  {
    if (this._bestModel == null) {
      return "";
    }
    String report = "";
    report = report + "Number of model segments: " + this._bestModel.size() + "\n";
    
    boolean reserves = false;
    double cumulative = getCumulativeProduction();
    for (int i = 0; i < this._bestModel.size(); i++)
    {
      DCAmodel dca = (DCAmodel)this._bestModel.get(i);
      if (i == this._bestModel.size() - 1) {
        reserves = true;
      }
      report = report + dca.report(reserves, cumulative);
      report = report + "\n";
    }
    if ((getLastSegmentModel().getReserves() != null) || 
      (getLastSegmentModel().getEURs() != null))
    {
      report = report + " Model EUR: " + getEUR() + "\n";
      report = report + " Final monthly rate: " + getEURrate() + "\n";
      report = report + " Date to EUR: " + getEURdate().toString() + "\n";
      report = report + " Month to EUR: " + getEURmonth();
    }
    return report;
  }
  
  public double getFitCC()
  {
    if (getModel() == null) {
      return NaN.0D;
    }
    double[] modeled = getModeledProduction();
    double[] model = getPrediction();
    boolean[] outliers = getOutliers();
    
    int n = 0;
    for (int i = 0; i < modeled.length; i++) {
      if ((modeled[i] > 0.0D) && (outliers[i] == 0)) {
        n++;
      }
    }
    if (n < 5) {
      return NaN.0D;
    }
    double[] x = new double[n];
    double[] y = new double[n];
    int i = 0;
    for (int j = 0; i < modeled.length; i++) {
      if ((modeled[i] > 0.0D) && (outliers[i] == 0))
      {
        x[j] = modeled[i];
        y[(j++)] = model[i];
      }
    }
    return DCAUtil.correlationCoefficient(x, y);
  }
  
  public double getFitRC()
  {
    if (getModel() == null) {
      return NaN.0D;
    }
    double[] modeled = getModeledProduction();
    double[] model = getPrediction();
    boolean[] outliers = getOutliers();
    
    int n = 0;
    for (int i = 0; i < modeled.length; i++) {
      if ((modeled[i] > 0.0D) && (outliers[i] == 0)) {
        n++;
      }
    }
    if (n < 5) {
      return NaN.0D;
    }
    double[] x = new double[n];
    double[] y = new double[n];
    int i = 0;
    for (int j = 0; i < modeled.length; i++) {
      if ((modeled[i] > 0.0D) && (outliers[i] == 0))
      {
        x[j] = modeled[i];
        y[(j++)] = model[i];
      }
    }
    return DCAUtil.correlationCoefficient(DCAUtil.rank(x), DCAUtil.rank(y));
  }
  
  private void copyModelingParameters(DCAdriver driver)
  {
    setSegmentation(driver.getSegmentation(), driver.getSegmentThreshold());
    setModelExtremes("Arps", this._arpsLoExtremes, this._arpsHiExtremes);
    setModelExtremes("SEPD", this._sepdLoExtremes, this._sepdHiExtremes);
    setModelExtremes("Duong", this._duongLoExtremes, this._duongHiExtremes);
    setModelExtremes("Logistic Growth", this._lgmLoExtremes, this._lgmHiExtremes);
    
    List<DCAmodel> models = driver.getModelTypes();
    ArrayList<String> types = new ArrayList(models.size());
    DCAmodel model;
    for (Iterator localIterator = models.iterator(); localIterator.hasNext(); types.add(model.getType())) {
      model = (DCAmodel)localIterator.next();
    }
    setModelTypes(types);
    setRegressionScaling(driver.getRegressionScaling());
    setOutlierAnalysis(driver.getOutlierAnalysis(), driver.getOutlierThreshold());
    setMinimizeRate(driver.getMinimizeRate());
    setMinimizeCumulative(driver.getMinimizeCumulative());
    setSearchDecline(driver.getSearchDecline());
    setNominalDecline(driver.getNominalDecline());
    setTailDecline(driver.getTailDecline());
    setMinimumDecline(driver.getMinimumDecline());
    setTransitionMonth(driver.getTransitionMonth());
    setTransitionSlope(driver.getTransitionSlope());
    
    setIOunits(driver.getIOunits());
  }
  
  static String monthYear(Date date)
  {
    String fullDate = date.toString();
    String monthYear = "";
    int day = Integer.valueOf(fullDate.substring(8, 10)).intValue();
    String month = fullDate.substring(5, 7);
    String year = fullDate.substring(0, 4);
    
    boolean newYear = false;
    if (day > 15) {
      if (month.equals("01"))
      {
        month = "Feb";
      }
      else if (month.equals("02"))
      {
        month = "Mar";
      }
      else if (month.equals("03"))
      {
        month = "Apr";
      }
      else if (month.equals("04"))
      {
        month = "May";
      }
      else if (month.equals("05"))
      {
        month = "Jun";
      }
      else if (month.equals("06"))
      {
        month = "Jul";
      }
      else if (month.equals("07"))
      {
        month = "Aug";
      }
      else if (month.equals("08"))
      {
        month = "Sep";
      }
      else if (month.equals("09"))
      {
        month = "Oct";
      }
      else if (month.equals("10"))
      {
        month = "Nov";
      }
      else if (month.equals("11"))
      {
        month = "Dec";
      }
      else if (month.equals("12"))
      {
        month = "Jan";newYear = true;
      }
    }
    if (newYear) {
      year = "" + (Integer.valueOf(year).intValue() + 1);
    }
    monthYear = monthYear + month + " " + year;
    return monthYear;
  }
  
  public static void main(String[] args)
  {
    int[] datesI = { 954568800, 957157200, 959835600, 962427600, 965106000, 967784400, 970376400, 973058400, 975650400, 978328800, 981007200, 983426400, 986104800, 988693200, 991371600, 993963600, 996642000, 999320400, 1001912400, 1004594400, 1007186400, 1009864800, 1012543200, 1014962400, 1017640800, 1020229200, 1022907600, 1025499600, 1028178000, 1030856400, 1033448400, 1036130400, 1038722400, 1041400800, 1044079200, 1046498400, 1049176800, 1051765200, 1054443600, 1057035600, 1059714000, 1062392400, 1064984400, 1067666400, 1070258400, 1072936800, 1075615200, 1078120800, 1080799200, 1083387600, 1086066000, 1088658000, 1091336400, 1094014800, 1096606800, 1099288800, 1101880800, 1104559200, 1107237600, 1109656800, 1112335200, 1114923600, 1117602000, 1120194000, 1122872400, 1125550800, 1128142800, 1130824800, 1133416800, 1136095200, 1138773600, 1141192800, 1143871200, 1146459600, 1149138000, 1151730000, 1154408400, 1157086800, 1159678800, 1162360800, 1164952800, 1167631200, 1170309600, 1172728800, 1175403600, 1177995600, 1180674000, 1183266000, 1185944400, 1188622800, 1191214800, 1193893200, 1196488800, 1199167200, 1201845600, 1204351200, 1207026000, 1209618000, 1212296400, 1214888400, 1217566800, 1220245200, 1222837200, 1225515600, 1228111200, 1230789600, 1233468000, 1235887200, 1238562000, 1241154000, 1243832400, 1246424400, 1249102800, 1251781200, 1254373200, 1257051600, 1259647200, 1262325600, 1265004000, 1267423200, 1270098000, 1272690000, 1275368400, 1277960400, 1280638800, 1283317200, 1285909200, 1288587600, 1291183200, 1293861600, 1296540000, 1298959200, 1301634000, 1304226000, 1306904400, 1309496400, 1312174800, 1314853200, 1317445200, 1320123600, 1322719200, 1325397600, 1328076000, 1330581600, 1333256400, 1335848400, 1338526800, 1341118800, 1343797200, 1346475600, 1349067600, 1351746000, 1354341600, 1357020000, 1359698400, 1362117600, 1364792400, 1367384400, 1370062800, 1372654800, 1375333200, 1378011600, 1380603600, 1383282000, 1385877600, 1388556000, 1391234400, 1393653600, 1396328400, 1398920400, 1401598800, 1404190800, 1406869200, 1409547600, 1412139600, 1414818000, 1417413600, 1420092000, 1422770400, 1425189600, 1427864400, 1430456400, 1433134800, 1435726800, 1438405200, 1441083600, 1443675600, 1446354000, 1448949600, 1451628000, 1454306400, 1456812000, 1459486800, 1462078800, 1464757200, 1467349200, 1470027600, 1472706000 };
    
    double[] streamI = { 33891.0D, 23500.0D, 24942.0D, 29860.0D, 17730.0D, 21227.0D, 19009.0D, 18594.0D, 22693.0D, 19636.0D, 16153.0D, 15336.0D, 14469.0D, 10328.0D, 16036.0D, 13496.0D, 10989.0D, 11251.0D, 9716.0D, 10511.0D, 11184.0D, 11026.0D, 10405.0D, 11004.0D, 1926.0D, 1990.0D, 8537.0D, 5153.0D, 5804.0D, 5693.0D, 5685.0D, 6339.0D, 6426.0D, 6167.0D, 6522.0D, 6856.0D, 6437.0D, 6623.0D, 6388.0D, 6537.0D, 6007.0D, 6400.0D, 6293.0D, 4355.0D, 5501.0D, 5387.0D, 5387.0D, 5632.0D, 5402.0D, 5732.0D, 5345.0D, 5418.0D, 5302.0D, 5046.0D, 5117.0D, 5231.0D, 5343.0D, 5262.0D, 4595.0D, 4772.0D, 4574.0D, 4703.0D, 4534.0D, 4746.0D, 4808.0D, 4194.0D, 4778.0D, 4509.0D, 4526.0D, 4495.0D, 3972.0D, 4440.0D, 4243.0D, 4399.0D, 4222.0D, 4105.0D, 4155.0D, 3843.0D, 3937.0D, 3892.0D, 3556.0D, 4096.0D, 3295.0D, 3802.0D, 3693.0D, 3715.0D, 3633.0D, 3801.0D, 3741.0D, 3735.0D, 3860.0D, 3677.0D, 3983.0D, 3904.0D, 3579.0D, 3598.0D, 3782.0D, 3811.0D, 3628.0D, 3514.0D, 3763.0D, 3538.0D, 3665.0D, 3531.0D, 3807.0D, 3648.0D, 3177.0D, 3586.0D, 3436.0D, 3549.0D, 3401.0D, 3444.0D, 3527.0D, 3499.0D, 3484.0D, 3325.0D, 3350.0D, 3164.0D, 3221.0D, 3370.0D, 3211.0D, 2897.0D, 3142.0D, 3341.0D, 3193.0D, 3117.0D, 3184.0D, 3067.0D, 3126.0D, 2956.0D, 2650.0D, 2976.0D, 2875.0D, 2732.0D, 3039.0D, 2729.0D, 2948.0D, 2722.0D, 2858.0D, 2791.0D, 2814.0D, 2726.0D, 2593.0D, 2789.0D, 2599.0D, 2769.0D, 2521.0D, 2658.0D, 2666.0D, 2565.0D, 2653.0D, 2481.0D, 2628.0D, 2600.0D, 2368.0D, 2625.0D, 2535.0D, 2605.0D, 2530.0D, 2563.0D, 2573.0D, 2460.0D, 2433.0D, 2533.0D, 2614.0D, 2547.0D, 2007.0D, 1836.0D, 1932.0D, 1942.0D, 2119.0D, 2164.0D, 1841.0D, 1901.0D, 1508.0D, 2328.0D, 2467.0D, 2378.0D, 2075.0D, 2343.0D, 2234.0D, 2274.0D, 2219.0D, 2291.0D, 2284.0D, 2139.0D, 2153.0D, 2148.0D, 2025.0D, 2065.0D, 1941.0D, 2075.0D, 2056.0D, 1871.0D, 2106.0D, 2201.0D, 2135.0D, 2140.0D };
    
    long[] dates1 = { 704700000000L, 707378400000L, 709970400000L, 712648800000L, 715327200000L, 717919200000L, 720597600000L, 723189600000L, 725868000000L, 728546400000L, 730965600000L, 733644000000L, 736236000000L, 738914400000L, 741506400000L, 744184800000L, 746863200000L, 749455200000L, 752133600000L, 754725600000L, 757404000000L, 760082400000L, 762501600000L, 765180000000L, 767772000000L, 770450400000L, 773042400000L, 775720800000L, 778399200000L, 780991200000L, 783669600000L, 786261600000L, 788940000000L, 791618400000L, 794037600000L, 796716000000L, 799308000000L, 801986400000L };
    
    double[] stream1 = { 2114.0D, 2107.0D, 2084.0D, 1524.0D, 1563.0D, 1666.0D, 1564.0D, 1388.0D, 1447.0D, 1174.0D, 1275.0D, 1134.0D, 1177.0D, 967.0D, 939.0D, 666.0D, 739.0D, 742.0D, 820.0D, 691.0D, 808.0D, 775.0D, 895.0D, 639.0D, 686.0D, 505.0D, 524.0D, 576.0D, 366.0D, 388.0D, 553.0D, 556.0D, 610.0D, 259.0D, 150.0D, 182.0D, 204.0D, 72.0D };
    
    String unit1 = "Mcf";
    
    ArrayList<String> types = new ArrayList();types.add("Arps");
    
    DCAdriver _dcaDriver = new DCAdriver(datesI, streamI, unit1);
    _dcaDriver.setModelTypes(types);
    _dcaDriver.model();
    DCAmodel model = _dcaDriver.getLastSegmentModel();
    double[] parms = model.getModel();
    System.out.println(model.getType() + "," + parms[0] + "," + parms[1] + "," + parms[2]);
    
    DCAmodel _dcaModel = new DCAarps();
    _dcaModel.setProduction(streamI, unit1);
    _dcaModel.setTailDecline("Natural");
    _dcaModel.autoFit(true);
    parms = _dcaModel.getModel();
    System.out.println(_dcaModel.getType() + "," + parms[0] + "," + parms[1] + "," + parms[2]);
  }
}
