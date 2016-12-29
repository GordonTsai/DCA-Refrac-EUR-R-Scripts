package com.drillinginfo.dca;

import java.util.Random;

public abstract class DCAmodel
{
  protected String _ioUnits = "m3";
  protected double[] _production;
  protected double[] _modelProduction;
  protected double[] _modelMonths;
  protected double[] _predicted;
  protected boolean[] _outliers;
  protected int _endMonth;
  protected int _lastMonth;
  protected int _maxMonth = 1;
  protected double _cumulative;
  protected double _q0 = 0.0D;
  protected double _dcaParm1;
  protected double _dcaParm2;
  protected double _dcaParm3;
  protected double _dcaParm4;
  protected double _declineMonth;
  protected double _sseMin;
  protected double[] _dcaLo;
  protected double[] _dcaHi;
  protected int _hindcastMonth = 6;
  private double[][] _hindcastResults;
  protected int _numTrials = 1000;
  protected int _maxProducingMonths = 600;
  protected double _reserves;
  protected double _eur;
  protected double _predictionYears;
  protected double[] _totalReserves;
  protected double[] _totalEURs;
  protected double[][] _trialReserves;
  protected double[][] _trialEURs;
  protected double[][] _trialParms;
  protected int[] _trialIndices;
  protected int[] _eurMonths;
  protected String _type = "";
  protected String _regressionScale = "Linear";
  protected String _reservesReference = "First";
  protected String _tailDecline = "Natural";
  protected boolean _searchDecline = false;
  protected boolean _nominalDecline = true;
  protected double _transitionSlope = 0.05D;
  protected double _transitionMonth = Double.MAX_VALUE;
  protected double _minDecline = 0.05D;
  protected double _minProduction = 10.0D;
  protected long _seed = Long.MIN_VALUE;
  protected String _forecastFailure = "";
  protected boolean _useCumulative = false;
  protected boolean _useProduction = true;
  protected boolean _outlierAnalysis = true;
  protected double _outlierThreshold = 3.0D;
  protected int _imin;
  protected int _jmin;
  protected int _kmin;
  protected int _lmin;
  protected int _mmin;
  protected LMfunction _LMfunction;
  
  public String getType()
  {
    return this._type;
  }
  
  public LMfunction getLM()
  {
    return this._LMfunction;
  }
  
  public double[] getProduction()
  {
    double[] production = new double[this._production.length];
    for (int i = 0; i < this._production.length; i++) {
      production[i] = toIO(this._production[i]);
    }
    return production;
  }
  
  public double[] getCumulativeProduction()
  {
    double[] cums = new double[this._production.length];
    cums[0] = toIO(this._production[0]);
    int i = 1;
    for (int im1 = 0; i < this._production.length; im1++)
    {
      cums[im1] += toIO(this._production[i]);i++;
    }
    return cums;
  }
  
  public double[] getModeledProduction()
  {
    double[] modeled = null;
    if (this._modelMonths != null)
    {
      modeled = new double[this._production.length];
      for (int i = 0; i < this._modelProduction.length; i++)
      {
        int ii = (int)this._modelMonths[i] + this._maxMonth - 2;
        modeled[ii] = toIO(this._modelProduction[i]);
      }
      return modeled;
    }
    modeled = new double[this._modelProduction.length];
    for (int i = 0; i < this._modelProduction.length; i++) {
      modeled[i] = toIO(this._modelProduction[i]);
    }
    return modeled;
  }
  
  public boolean[] getOutliers()
  {
    return this._outliers;
  }
  
  public double[] getPrediction()
  {
    if (this._predicted == null) {
      return null;
    }
    double[] predicted = new double[this._production.length];
    for (int i = 0; i < predicted.length; i++) {
      predicted[i] = toIO(this._predicted[i]);
    }
    return predicted;
  }
  
  public double[] getCumulativePrediction()
  {
    if (this._predicted == null) {
      return null;
    }
    double[] cums = new double[this._predicted.length];
    cums[0] = toIO(this._predicted[0]);
    int i = 1;
    for (int im1 = 0; i < this._predicted.length; im1++)
    {
      cums[im1] += toIO(this._predicted[i]);i++;
    }
    return cums;
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
  
  public double[] getModel()
  {
    return new double[] { toIO(this._dcaParm1), this._dcaParm2, this._dcaParm3, this._dcaParm4, this._sseMin };
  }
  
  public double[] getLoExtremes()
  {
    return this._dcaLo;
  }
  
  public void setLoExtremes(double[] lo)
  {
    if (lo == null) {
      return;
    }
    this._dcaLo = ((double[])lo.clone());
  }
  
  public void setHiExtremes(double[] hi)
  {
    if (hi == null) {
      return;
    }
    this._dcaHi = ((double[])hi.clone());
  }
  
  public double[] getHiExtremes()
  {
    return this._dcaHi;
  }
  
  private void setModelExtremes()
  {
    double[] production = regressionProduction();
    double pmax = 0.0D;
    for (int i = 0; i < production.length; i++) {
      if (pmax < production[i]) {
        pmax = production[i];
      }
    }
    this._dcaHi[0] = (2.0D * pmax);
    this._dcaLo[0] = (pmax / 2.0D);
  }
  
  public String[] getNames()
  {
    return null;
  }
  
  public void setModel(double p1, double p2, double p3, double p4)
  {
    this._dcaParm1 = toSI(p1);
    this._dcaParm2 = p2;
    this._dcaParm3 = p3;
    this._dcaParm4 = p4;
  }
  
  public void setSearchDecline(boolean search)
  {
    this._searchDecline = search;
  }
  
  public void setNominalDecline(boolean decline)
  {
    this._nominalDecline = decline;
  }
  
  public void setTailDecline(String decline)
  {
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
  
  public void setTransitionMonth(double month)
  {
    if (month <= this._maxMonth) {
      this._transitionMonth = this._production.length;
    } else {
      this._transitionMonth = month;
    }
  }
  
  public void setMinimumDecline(double decline)
  {
    this._minDecline = (decline / 12.0D);
  }
  
  @Deprecated
  public void setDeclineMonth(double decline)
  {
    this._declineMonth = decline;
  }
  
  public double getDeclineMonth()
  {
    return this._transitionMonth;
  }
  
  public void setHindcastMonth(int month)
  {
    this._hindcastMonth = month;
  }
  
  public int getHindcastMonth()
  {
    return this._hindcastMonth;
  }
  
  public double[][] getHindcastResults()
  {
    return this._hindcastResults;
  }
  
  public void setMinimumProduction(double minimum)
  {
    this._minProduction = minimum;
  }
  
  public void setMaximumProducingMonths(int maximum)
  {
    this._maxProducingMonths = maximum;
    this._lastMonth = maximum;
  }
  
  public void setSeed(long seed)
  {
    this._seed = seed;
  }
  
  public void setTrialCount(int ntrials)
  {
    this._numTrials = ntrials;
  }
  
  public int getTrialCount()
  {
    return this._numTrials;
  }
  
  public double[] getTrialReserves()
  {
    return this._totalReserves;
  }
  
  public double[] getTrialEURs()
  {
    return this._totalEURs;
  }
  
  public int[] getTrialIndices()
  {
    return this._trialIndices;
  }
  
  public double[] getModelForecast()
  {
    int nmodel = 0;
    if (this._trialReserves != null) {
      nmodel = this._trialReserves[0].length;
    }
    if (this._trialEURs != null) {
      nmodel = this._trialEURs[0].length;
    }
    double[] model = new double[nmodel];
    for (int i = 0; i < model.length; i++)
    {
      double predict = toIO(model(this._endMonth + i + 1));
      model[i] = (predict < this._minProduction ? 0.0D : predict);
    }
    return model;
  }
  
  public double[][] getTrialParameters()
  {
    if (this._trialParms == null) {
      return (double[][])null;
    }
    double[][] trialParms = new double[this._trialParms.length][this._trialParms[0].length];
    for (int i = 0; i < trialParms[0].length; i++) {
      trialParms[0][i] = toIO(this._trialParms[0][i]);
    }
    for (int j = 1; j < trialParms.length; j++) {
      for (int i = 0; i < trialParms[j].length; i++) {
        trialParms[j][i] = this._trialParms[j][i];
      }
    }
    return trialParms;
  }
  
  public double[] getPCurve(double p)
  {
    if ((this._trialReserves == null) && (this._trialEURs == null)) {
      return null;
    }
    double[][] trials = (double[][])null;
    if (this._trialReserves != null) {
      trials = this._trialReserves;
    }
    if (this._trialEURs != null) {
      trials = this._trialEURs;
    }
    double[] pcurve = new double[trials[0].length];
    double[] reserves = new double[this._numTrials];
    int[] indices = new int[this._numTrials];
    
    int ip = (int)Math.round(p * this._numTrials);
    for (int i = 0; i < trials[0].length; i++)
    {
      for (int j = 0; j < this._numTrials; j++)
      {
        reserves[j] = trials[j][i];
        indices[j] = j;
      }
      DCAUtil.heapSort(reserves, indices);
      
      pcurve[i] = ((float)toIO(trials[indices[ip]][i]));
    }
    return pcurve;
  }
  
  public double[] getEURs()
  {
    if (this._totalEURs == null) {
      return null;
    }
    double[] eurs = new double[this._numTrials];
    int[] indices = new int[this._numTrials];
    for (int i = 0; i < this._numTrials; i++)
    {
      eurs[i] = this._totalEURs[i];
      indices[i] = i;
    }
    DCAUtil.heapSort(eurs, indices);
    for (int j = 0; j < this._numTrials; j++) {
      eurs[j] = toIO(eurs[j]);
    }
    return eurs;
  }
  
  public int[] getEURmonths()
  {
    return this._eurMonths;
  }
  
  public double[] getReserves()
  {
    if (this._trialReserves == null) {
      return null;
    }
    double[] reserves = new double[this._numTrials];
    int[] indices = new int[this._numTrials];
    for (int j = 0; j < this._numTrials; j++)
    {
      for (int i = 0; i < this._trialReserves[j].length; i++) {
        reserves[j] += this._trialReserves[j][i];
      }
      indices[j] = j;
    }
    DCAUtil.heapSort(reserves, indices);
    for (int j = 0; j < this._numTrials; j++) {
      reserves[j] = toIO(reserves[j]);
    }
    return reserves;
  }
  
  public double getReserves(double p)
  {
    if ((this._trialReserves == null) && (this._trialEURs == null)) {
      return NaN.0D;
    }
    double[] reserves = null;
    if (this._trialReserves != null) {
      reserves = getReserves();
    }
    if (this._trialEURs != null) {
      reserves = getEURs();
    }
    float[] cp = new float[reserves.length];
    for (int i = 1; i < cp.length; i++) {
      cp[i] = (i / (cp.length - 1));
    }
    int ip = cp.length - 1;
    for (int i = 0; i < cp.length; i++) {
      if (p < cp[i])
      {
        ip = i; break;
      }
    }
    double r = 0.0D;
    double w = cp[ip] - cp[(ip - 1)];
    if (w == 0.0D) {
      r = reserves[ip];
    } else {
      r = ((p - cp[(ip - 1)]) * reserves[ip] + (cp[ip] - p) * reserves[(ip - 1)]) / w;
    }
    return r;
  }
  
  public double getEstimatedReserves()
  {
    return toIO(computeReserves());
  }
  
  public double getEUR()
  {
    return toIO(computeEUR());
  }
  
  @Deprecated
  public void setProduction(double[] production)
  {
    setProduction(production, "m3");
  }
  
  public void setProduction(double[] production, String ioUnits)
    throws IllegalArgumentException
  {
    if ((!ioUnits.equals("bbl")) && (!ioUnits.equals("Mcf")))
    {
      String message = "Input units must be one of bbl, Mcf, or m3.";
      throw new IllegalArgumentException(message);
    }
    this._ioUnits = ioUnits;
    
    this._production = new double[production.length];
    for (int i = 0; i < production.length; i++) {
      if (!Double.isNaN(production[i])) {
        this._production[i] = toSI(production[i]);
      } else {
        this._production[i] = NaN.0D;
      }
    }
    this._endMonth = this._production.length;
    this._transitionMonth = this._production.length;
    this._maxMonth = 1;
    
    double pmax = this._production[0];
    this._modelProduction = new double[this._production.length];
    this._outliers = new boolean[this._production.length];this._outliers[0] = false;
    this._modelProduction[0] = this._production[0];
    for (int i = 1; i < this._production.length; i++)
    {
      this._outliers[i] = false;
      if (this._production[i] > 0.0D) {
        this._modelProduction[i] = this._production[i];
      } else {
        this._modelProduction[i] = NaN.0D;
      }
    }
    int nmonths = 0;int imonth = -1;
    for (int i = 0; i < this._production.length; i++)
    {
      if (this._production[i] > 0.0D)
      {
        nmonths++;
        if (imonth < 0) {
          imonth = i;
        }
      }
      if (nmonths == 20)
      {
        nmonths = i; break;
      }
    }
    if (imonth < 0)
    {
      String message = "No first non-zero production value within the first 20 months";
      throw new IllegalArgumentException(message);
    }
    for (int i = imonth; i < Math.min(this._production.length, nmonths); i++) {
      if (this._production[i] > pmax)
      {
        pmax = this._production[i];this._maxMonth = (i + 1);
      }
    }
    this._predicted = null;
  }
  
  private void setModeledProduction(double[] modeled)
  {
    this._modelProduction = modeled;
  }
  
  private void setOutliers(boolean[] outliers)
  {
    this._outliers = outliers;
  }
  
  public void setMaxMonth(int maxMonth)
  {
    this._maxMonth = maxMonth;
  }
  
  public void setOutlierAnalysis(boolean state, double threshold)
  {
    this._outlierAnalysis = state;
    this._outlierThreshold = threshold;
  }
  
  public void setRegressionScaling(String type)
  {
    this._regressionScale = type;
  }
  
  public void setForecastReference(String reference)
  {
    this._reservesReference = reference;
    this._lastMonth = ((int)(12.0D * this._predictionYears));
    if (this._reservesReference.equals("Last")) {
      this._lastMonth += this._endMonth;
    }
  }
  
  public void setForecastYears(double years)
  {
    this._predictionYears = years;
    this._lastMonth = ((int)(12.0D * this._predictionYears));
    if (this._reservesReference.equals("Last")) {
      this._lastMonth += this._endMonth;
    }
  }
  
  protected void setQo(double q0)
  {
    this._q0 = q0;
  }
  
  public double model(double t)
  {
    return model(this._dcaParm1, this._dcaParm2, this._dcaParm3, this._dcaParm4, t);
  }
  
  protected double model(double a, double b, double c, double d, double t)
  {
    return NaN.0D;
  }
  
  public double getModelFit()
  {
    double[] modeled = getModeledProduction();
    double avgpro = 0.0D;int navgpro = 0;
    for (int i = 0; i < modeled.length; i++) {
      if ((!Double.isNaN(modeled[i])) && (modeled[i] > 0.0D))
      {
        avgpro += modeled[i];navgpro++;
      }
    }
    if (navgpro == 0) {
      return NaN.0D;
    }
    avgpro /= navgpro;
    double avgerr = Math.sqrt(this._sseMin / this._modelProduction.length);
    return avgerr / avgpro;
  }
  
  public void autoFit(boolean coarse)
    throws IllegalStateException
  {
    int n = 0;
    boolean[] fail = new boolean[this._production.length];
    for (int i = this._maxMonth - 1; i < this._production.length; i++)
    {
      fail[i] = ((Double.isNaN(this._production[i])) || (this._production[i] <= 0.0D) || (this._outliers[i] != 0) ? 1 : false);
      if (fail[i] == 0) {
        n++;
      }
    }
    this._modelProduction = new double[n];
    this._modelMonths = new double[n];
    int i = this._maxMonth - 1;
    for (int j = 0; i < this._production.length; i++) {
      if (fail[i] == 0)
      {
        this._modelProduction[j] = this._production[i];
        this._modelMonths[(j++)] = (i - this._maxMonth + 2);
      }
    }
    if (coarse)
    {
      setModelExtremes();
      try
      {
        searchInitial();
      }
      catch (IllegalStateException exp)
      {
        throw exp;
      }
    }
    LM lm = new LM();
    LMfunction dcaLM = getLM();
    double[] a = { this._dcaParm1, this._dcaParm2, this._dcaParm3 };
    if (getNames().length == 4) {
      a = new double[] { this._dcaParm1, this._dcaParm2, this._dcaParm3, this._dcaParm4 };
    }
    a = lm.solve(this._modelMonths, this._modelProduction, a, dcaLM);
    this._dcaParm1 = a[0];this._dcaParm2 = a[1];this._dcaParm3 = a[2];
    if (getNames().length == 4) {
      this._dcaParm4 = a[3];
    }
    search();
    
    this._predicted = new double[this._production.length];
    for (int l = 0; l < this._predicted.length; l++) {
      this._predicted[l] = Math.max(0.0D, model(l + 1));
    }
  }
  
  protected void search() {}
  
  protected void searchInitial() {}
  
  public void hindcast()
  {
    this._hindcastResults = ((double[][])null);
    if (this._hindcastMonth < 4) {
      return;
    }
    int ncurves = this._production.length - this._hindcastMonth;
    if (ncurves <= 0) {
      return;
    }
    this._hindcastResults = new double[8][ncurves];
    
    int lmonth = this._hindcastMonth + 1;
    for (int i = 0; i < ncurves; i++)
    {
      double[] production = new double[lmonth];
      for (int j = 0; j < lmonth; j++) {
        production[j] = this._production[j];
      }
      DCAmodel model = new DCAarps();
      try
      {
        model.setProduction(production, this._ioUnits);
        model.setLoExtremes(getLoExtremes());
        model.setHiExtremes(getHiExtremes());
        model.setMinimumProduction(30.0D);
        model.setTailDecline("Natural");
        
        model.autoFit(true);
        this._hindcastResults[0][i] = model.getModel()[0];
        this._hindcastResults[1][i] = model.getModel()[1];
        this._hindcastResults[2][i] = model.getModel()[2];
        this._hindcastResults[3][i] = model.getModel()[3];
        this._hindcastResults[4][i] = model.getSecantDecline();
        this._hindcastResults[5][i] = model.getEUR();
        this._hindcastResults[6][i] = model.getFitCC();
        this._hindcastResults[7][i] = model.getFitRC();
      }
      catch (IllegalArgumentException|IllegalStateException exp)
      {
        for (int j = 0; j < this._hindcastResults.length; j++) {
          this._hindcastResults[j][i] = NaN.0D;
        }
      }
      lmonth++;
      if (lmonth > this._production.length) {
        break;
      }
    }
  }
  
  public void bayesian()
    throws IllegalStateException
  {
    Random random = computeBayesianTrials();
    if (this._reservesReference.equals("EUR"))
    {
      this._trialReserves = ((double[][])null);this._totalReserves = null;
      computeProbabilisticEURs(random);
    }
    else
    {
      this._totalEURs = null;this._eurMonths = null;
      computeProbabilisticReserves(random);
    }
  }
  
  protected Random computeBayesianTrials()
  {
    Random random = new Random(System.currentTimeMillis());
    if (this._seed > Long.MIN_VALUE) {
      random = new Random(this._seed);
    }
    int np = getNames().length;
    double[] trial = new double[4];
    this._trialParms = new double[4][this._numTrials];
    
    double p1 = this._trialParms[0][0] = this._dcaParm1;
    double p2 = this._trialParms[1][0] = this._dcaParm2;
    double p3 = this._trialParms[2][0] = this._dcaParm3;
    double p4 = this._trialParms[3][0] = this._dcaParm4;
    
    double sum = 0.0D;double sum2 = 0.0D;int nsum = 0;
    for (int i = 0; i < this._modelMonths.length; i++)
    {
      double predicted = model(this._modelMonths[i]);
      double residual = (this._modelProduction[i] - predicted) / predicted;
      sum += residual;sum2 += residual * residual;nsum++;
    }
    double rsd = Math.sqrt((sum2 - sum * sum / nsum) / nsum);
    if ((rsd == 0.0D) || (Double.isNaN(rsd)))
    {
      forecastFailed("No model error variance for bayesian forecast.", p1, p2, p3, p4);
      throw new IllegalStateException(this._forecastFailure);
    }
    double[] hi = new double[np];
    double[] lo = new double[np];
    double[] sd = new double[np];
    for (int i = 0; i < np; i++)
    {
      sd[i] = (rsd * this._trialParms[i][0]);
      lo[i] = Double.max(this._trialParms[i][0] - sd[i], this._dcaLo[i]);
      hi[i] = Double.min(this._trialParms[i][0] + sd[i], this._dcaHi[i]);
    }
    int n = this._modelMonths.length;
    if ((Double.isNaN(this._sseMin)) || (this._sseMin == 0.0D)) {
      this._sseMin = sumSquaredErrors();
    }
    double variance = this._sseMin / n;
    double ltm1 = likelihood(this._trialParms[0][0], this._trialParms[1][0], this._trialParms[2][0], this._trialParms[3][0], variance);
    double ptm1 = 1.0D;
    for (int j = 0; j < np; j++) {
      ptm1 = ptm1 * (forwardGaussian(hi[j], this._trialParms[j][0], sd[j]) - forwardGaussian(lo[j], this._trialParms[j][0], sd[j]));
    }
    double lptm1 = Math.exp(ltm1) * ptm1;
    
    int i = 0;
    boolean nextTrial = true;int ntotalTrials = 0;
    while (nextTrial)
    {
      ntotalTrials++;
      for (int j = 0; j < np; j++)
      {
        boolean draw = true;int ndraws = 0;
        while (draw)
        {
          ndraws++;
          double p = backwardGaussian(random.nextDouble(), this._trialParms[j][i], sd[j]);
          draw = (p > hi[j]) || (p < lo[j]);
          if (!draw) {
            trial[j] = p;
          }
          if (ndraws == 10)
          {
            lo[j] += random.nextDouble() * (hi[j] - lo[j]);
            ndraws = 0;draw = false;
          }
        }
      }
      double lt = likelihood(trial[0], trial[1], trial[2], trial[3], variance);
      double pt = 1.0D;
      for (int j = 0; j < np; j++) {
        pt = pt * (forwardGaussian(hi[j], trial[j], sd[j]) - forwardGaussian(lo[j], trial[j], sd[j]));
      }
      double lpt = Math.exp(lt) * pt;
      
      double alpha = lpt / lptm1;
      if (random.nextDouble() < alpha)
      {
        i++;
        for (int j = 0; j < trial.length; j++) {
          this._trialParms[j][i] = trial[j];
        }
        lptm1 = lpt;
        if (i == this._numTrials - 1) {
          nextTrial = false;
        }
      }
      if (ntotalTrials > this._numTrials * 1000)
      {
        forecastFailed("Bayesian forecast failed to converge.", p1, p2, p3, p4);
        throw new IllegalStateException(this._forecastFailure);
      }
    }
    this._dcaParm1 = p1;
    this._dcaParm2 = p2;
    this._dcaParm3 = p3;
    this._dcaParm4 = p4;
    
    return random;
  }
  
  private void computeProbabilisticEURs(Random random)
  {
    this._trialIndices = new int[this._numTrials];
    this._totalEURs = new double[this._numTrials];
    this._eurMonths = new int[this._numTrials];
    
    double minProduction = toSI(this._minProduction);
    double[][] trialEURs = new double[this._numTrials][this._maxProducingMonths - this._endMonth + 1];
    for (int i = 0; i < this._numTrials; i++)
    {
      int j = this._endMonth;
      for (int jj = 0; j < this._maxProducingMonths + 1; jj++)
      {
        trialEURs[i][jj] = model(this._trialParms[0][i], this._trialParms[1][i], this._trialParms[2][i], this._trialParms[3][i], j);
        if (trialEURs[i][jj] < minProduction)
        {
          this._eurMonths[i] = j;
          break;
        }
        this._totalEURs[i] += trialEURs[i][jj];
        if (j == this._maxProducingMonths) {
          this._eurMonths[i] = this._maxProducingMonths;
        }
        j++;
      }
      this._trialIndices[i] = i;
    }
    int maxMonths = 0;
    for (int i = 0; i < this._eurMonths.length; i++) {
      if (this._eurMonths[i] > maxMonths) {
        maxMonths = this._eurMonths[i];
      }
    }
    this._trialEURs = new double[this._numTrials][maxMonths - this._endMonth + 1];
    for (int i = 0; i < this._numTrials; i++) {
      for (int j = 0; j < this._trialEURs[i].length; j++) {
        this._trialEURs[i][j] = trialEURs[i][j];
      }
    }
  }
  
  private void computeProbabilisticReserves(Random random)
  {
    this._trialIndices = new int[this._numTrials];
    this._lastMonth = ((int)(12.0D * this._predictionYears));
    if (this._reservesReference.equals("Last")) {
      this._lastMonth += this._endMonth;
    }
    this._trialReserves = new double[this._numTrials][this._lastMonth - this._endMonth + 1];
    this._totalReserves = new double[this._numTrials];
    for (int i = 0; i < this._numTrials; i++)
    {
      int j = this._production.length;
      for (int jj = 0; j <= this._lastMonth; jj++)
      {
        this._trialReserves[i][jj] = model(this._trialParms[0][i], this._trialParms[1][i], this._trialParms[2][i], this._trialParms[3][i], j);
        if (toIO(this._trialReserves[i][jj]) < this._minProduction) {
          this._trialReserves[i][jj] = 0.0D;
        }
        this._totalReserves[i] += this._trialReserves[i][jj];j++;
      }
      this._trialIndices[i] = i;
    }
    double sum = 0.0D;double sum2 = 0.0D;double nsum = 0.0D;
    for (int ip = 0; ip < this._totalReserves.length; ip++) {
      if (this._totalReserves[ip] > 0.0D)
      {
        sum += this._totalReserves[ip];
        sum2 += this._totalReserves[ip] * this._totalReserves[ip];
        nsum += 1.0D;
      }
    }
    double outlier = 2.0D;
    double mean = sum / nsum;
    double stdv = Math.sqrt((sum2 - sum * sum / nsum) / nsum);
    outlier = mean + outlier * stdv;
    
    int noutliers = 0;
    for (int ip = 0; ip < this._totalReserves.length; ip++) {
      if (this._totalReserves[ip] > outlier) {
        noutliers++;
      }
    }
    if (noutliers > 0)
    {
      int[] outliers = new int[noutliers];
      int ip = 0;
      for (int j = 0; ip < this._totalReserves.length; ip++) {
        if (this._totalReserves[ip] > outlier)
        {
          outliers[j] = ip;
          j++;
        }
      }
      for (int ip = 0; ip < this._totalReserves.length; ip++) {
        if (this._totalReserves[ip] > outlier)
        {
          int jp = this._totalReserves.length + 1;
          while (jp > this._totalReserves.length - 1)
          {
            jp = (int)Math.round(random.nextDouble() * this._totalReserves.length);
            if (jp == 0) {
              jp = 1;
            }
            for (int j = 0; j < noutliers; j++) {
              if (jp == outliers[j])
              {
                jp = this._totalReserves.length + 1;
                break;
              }
            }
          }
          this._totalReserves[ip] = this._totalReserves[jp];
          int j = this._production.length;
          for (int jj = 0; j <= this._lastMonth; jj++)
          {
            this._trialReserves[ip][jj] = this._trialReserves[jp][jj];j++;
          }
          for (int j = 0; j < this._trialParms.length; j++) {
            this._trialParms[j][ip] = this._trialParms[j][jp];
          }
        }
      }
    }
    DCAUtil.heapSort(this._totalReserves, this._trialIndices);
    this._forecastFailure = "";
  }
  
  private void forecastFailed(String reason, double p1, double p2, double p3, double p4)
  {
    this._forecastFailure = reason;
    this._trialParms = ((double[][])null);
    this._trialIndices = null;
    this._trialReserves = ((double[][])null);
    this._totalReserves = null;
    
    this._dcaParm1 = p1;
    this._dcaParm2 = p2;
    this._dcaParm3 = p3;
    this._dcaParm4 = p4;
  }
  
  protected boolean outliers()
  {
    if (!this._outlierAnalysis) {
      return false;
    }
    double[] modeled = getModeledProduction();
    double[] predicted = getPrediction();
    double[] residuals = new double[modeled.length];
    for (int i = 0; i < predicted.length; i++) {
      if (modeled[i] > 0.0D) {
        modeled[i] -= predicted[i];
      }
    }
    int n = 0;
    double mean = 0.0D;double stdev = 0.0D;
    for (int i = 0; i < residuals.length; i++) {
      if (modeled[i] > 0.0D)
      {
        mean += residuals[i];stdev += residuals[i] * residuals[i];n++;
      }
    }
    stdev = Math.sqrt((stdev - mean * mean / n) / n);
    mean /= n;
    
    n = 0;
    double min = mean - this._outlierThreshold * stdev;
    double max = mean + this._outlierThreshold * stdev;
    for (int i = 0; i < residuals.length; i++) {
      if (((residuals[i] < min) || (residuals[i] > max)) && (i + 1 > this._maxMonth) && (residuals[i] != 0.0D))
      {
        this._outliers[i] = true;n++;
      }
    }
    if (n > 0)
    {
      autoFit(true);return true;
    }
    return false;
  }
  
  public double cumulativeProduction()
  {
    double cumulative = 0.0D;
    for (int i = 0; i < this._production.length; i++) {
      if (!Double.isNaN(this._production[i])) {
        cumulative += this._production[i];
      }
    }
    return cumulative;
  }
  
  public double computeReserves()
  {
    double reserves = 0.0D;
    double minProduction = toSI(this._minProduction);
    for (int i = this._endMonth; i < this._lastMonth; i++)
    {
      double predict = model(i + 1);
      if (predict < minProduction)
      {
        reserves += predict;
        break;
      }
      reserves += predict;
    }
    return reserves;
  }
  
  public double computeEUR()
  {
    double reserves = computeReserves();
    
    double cumulative = cumulativeProduction();
    
    return reserves + cumulative;
  }
  
  public double getEURrate()
  {
    double predict = NaN.0D;
    double minProduction = toSI(this._minProduction);
    for (int i = this._endMonth; i < this._lastMonth; i++)
    {
      predict = model(i + 1);
      if (predict < minProduction) {
        return toIO(predict);
      }
    }
    return toIO(predict);
  }
  
  public int getEURmonth()
  {
    double predict = NaN.0D;
    double minProduction = toSI(this._minProduction);
    for (int i = this._endMonth; i < this._lastMonth; i++)
    {
      predict = model(i + 1);
      if (predict < minProduction) {
        return i;
      }
    }
    return -1;
  }
  
  protected double[] regressionProduction()
  {
    double[] production = new double[this._modelProduction.length];
    for (int i = 0; i < production.length; i++) {
      production[i] = this._modelProduction[i];
    }
    if (this._regressionScale.equals("Logarithmic")) {
      for (int i = 0; i < production.length; i++) {
        if (!Double.isNaN(production[i])) {
          production[i] = Math.log(production[i]);
        }
      }
    }
    return production;
  }
  
  protected double[] regressionCumProduction()
  {
    double[] production = new double[this._modelProduction.length];
    for (int i = 0; i < production.length; i++) {
      production[i] = this._modelProduction[i];
    }
    double[] cums = new double[production.length];
    if (!Double.isNaN(production[0])) {
      cums[0] = production[0];
    }
    int i = 1;
    for (int im1 = 0; i < production.length; im1++)
    {
      cums[im1] += production[i];i++;
    }
    if (this._regressionScale.equals("Logarithmic")) {
      for (int i = 0; i < production.length; i++) {
        cums[i] = Math.log(cums[i]);
      }
    }
    return cums;
  }
  
  protected double regressionError(double production, double predicted)
  {
    double err = 0.0D;
    if (this._regressionScale.equals("Linear")) {
      err = production - predicted;
    } else if (this._regressionScale.equals("Logarithmic")) {
      err = production - Math.log(predicted);
    }
    return err;
  }
  
  protected double sumSquaredErrors()
  {
    double sse = 0.0D;
    if (this._modelMonths == null)
    {
      int n = 0;
      for (int i = this._maxMonth - 1; i < this._production.length; i++) {
        if (this._outliers[i] == 0) {
          n++;
        }
      }
      this._modelMonths = new double[n];
      this._modelProduction = new double[n];
      int i = 0;
      for (int j = this._maxMonth - 1; j < this._production.length; j++) {
        if (this._outliers[i] == 0)
        {
          this._modelMonths[i] = i;
          this._modelProduction[(i++)] = this._production[j];
        }
      }
    }
    int n = Math.min(this._modelProduction.length, this._modelMonths.length);
    for (int i = 0; i < n; i++)
    {
      double predicted = model(this._modelMonths[i] + this._maxMonth - 1.0D);
      double err = this._modelProduction[i] - predicted;
      sse += err * err;
    }
    return sse;
  }
  
  public String getForecastFailure()
  {
    return this._forecastFailure;
  }
  
  public String report(boolean reserves)
  {
    return report(reserves, 0.0D);
  }
  
  public String report(boolean reserves, double cumulative)
  {
    int noutliers = 0;
    for (int i = 0; i < this._outliers.length; i++) {
      if (this._outliers[i] != 0) {
        noutliers++;
      }
    }
    this._sseMin = sumSquaredErrors();
    
    String report = "";
    report = report + "    Secant decline:     " + getSecantDecline() + "\n";
    report = report + "    Number of months:   " + this._production.length + "\n";
    report = report + "    Number of outliers: " + noutliers + "\n";
    report = report + "    Correlation coeff:  " + getFitCC() + "\n";
    report = report + "    Rank correl coeff:  " + getFitRC() + "\n";
    if ((reserves) && ((this._trialReserves != null) || (this._trialEURs != null)))
    {
      report = report + "\n";
      String type = "";
      double r = getEstimatedReserves();
      if (this._trialReserves != null)
      {
        type = "remaining recoverable estimate";
        report = report + " Model " + type + ": " + getEstimatedReserves() + "\n";
      }
      r = getReserves(0.1D);
      if (this._trialEURs != null) {
        r += cumulative;
      }
      report = report + " P90 " + type + ": " + r + "\n";
      r = getReserves(0.3D);
      if (this._trialEURs != null) {
        r += cumulative;
      }
      report = report + " P70 " + type + ": " + r + "\n";
      r = getReserves(0.5D);
      if (this._trialEURs != null) {
        r += cumulative;
      }
      report = report + " P50 " + type + ": " + r + "\n";
      r = getReserves(0.7D);
      if (this._trialEURs != null) {
        r += cumulative;
      }
      report = report + " P30 " + type + ": " + r + "\n";
      r = getReserves(0.9D);
      if (this._trialEURs != null) {
        r += cumulative;
      }
      report = report + " P10 " + type + ": " + r + "\n";
    }
    return report;
  }
  
  public double toIO(double valSI)
  {
    double valIO = valSI;
    if (this._ioUnits.equals("bbl")) {
      valIO = valSI / DCAUtil._bbl2m3;
    }
    if (this._ioUnits.equals("Mcf")) {
      valIO = valSI / DCAUtil._Mcf2m3;
    }
    return valIO;
  }
  
  public double toSI(double valIO)
  {
    double valSI = valIO;
    if (this._ioUnits.equals("bbl")) {
      valSI *= DCAUtil._bbl2m3;
    }
    if (this._ioUnits.equals("Mcf")) {
      valSI *= DCAUtil._Mcf2m3;
    }
    return valSI;
  }
  
  protected double likelihood(double a, double b, double c, double d, double variance)
  {
    this._dcaParm1 = a;this._dcaParm2 = b;this._dcaParm3 = c;this._dcaParm4 = d;
    double sse = sumSquaredErrors();
    return -(sse / this._modelMonths.length) / variance;
  }
  
  private double forwardGaussian(double v, double mean, double stdev)
  {
    double x = (v - mean) / stdev;
    int neg = x < 0.0D ? 1 : 0;
    if (neg == 1) {
      x *= -1.0D;
    }
    double k = 1.0D / (1.0D + 0.2316419D * x);
    double y = ((((1.330274429D * k - 1.821255978D) * k + 1.781477937D) * k - 0.356563782D) * k + 0.31938153D) * k;
    
    y = 1.0D - 0.398942280401D * Math.exp(-0.5D * x * x) * y;
    return (1 - neg) * y + neg * (1.0D - y);
  }
  
  private double backwardGaussian(double p, double mean, double stdev)
  {
    double a0 = 2.30753D;double a1 = 0.27061D;double b1 = 0.99229D;double b2 = 0.04481D;
    
    p = 1.0D - p;
    double t2 = 0.0D;
    if (p > 0.5D)
    {
      p = 1.0D - p;
      if ((Math.abs(p) > 1.0D) || (p < 1.0E-4D)) {
        p = 1.0E-4D;
      }
      double t1 = Math.sqrt(Math.log(1.0D / (p * p)));
      t2 = -(t1 - (2.30753D + 0.27061D * t1) / (1.0D + 0.99229D * t1 + 0.04481D * t1 * t1));
    }
    else
    {
      if (p < 1.0E-4D) {
        p = 1.0E-4D;
      }
      double t1 = Math.sqrt(Math.log(1.0D / (p * p)));
      t2 = t1 - (2.30753D + 0.27061D * t1) / (1.0D + 0.99229D * t1 + 0.04481D * t1 * t1);
    }
    return t2 * stdev + mean;
  }
  
  protected boolean terminalDecline(double[] exps, double[] qis)
  {
    boolean improved = false;
    if ((this._tailDecline.equals("Natural")) || (this._modelMonths.length < 7)) {
      improved = naturalDecline(exps, qis);
    } else if (this._searchDecline) {
      improved = searchDecline(exps, qis);
    } else {
      improved = nominalFixedDecline(exps, qis);
    }
    return improved;
  }
  
  private boolean searchDecline(double[] exps, double[] qis)
  {
    boolean improved = false;
    for (int m = 0; m < this._modelMonths.length - 1; m++)
    {
      double dmin = this._minDecline;
      double[] exp = new double[this._modelMonths.length];
      if (this._tailDecline.equals("Exponential")) {
        dmin = Math.log(exps[m] / exps[(m + 1)]);
      }
      for (int l = m + 1; l < this._modelMonths.length; l++) {
        exps[m] *= Math.exp(dmin * (this._modelMonths[m] - this._modelMonths[l]));
      }
      for (int i = 0; i < qis.length; i++)
      {
        double sse = 0.0D;double q = 0.0D;
        for (int l = 0; l <= m; l++)
        {
          q = qis[i] * exps[l];
          double err = this._modelProduction[l] - q;
          sse += err * err;
        }
        for (int l = m + 1; l < this._modelMonths.length; l++)
        {
          q = qis[i] * exp[l];
          double err = this._modelProduction[l] - q;
          sse += err * err;
        }
        if (sse < this._sseMin)
        {
          this._sseMin = sse;this._mmin = m;this._imin = i;this._minDecline = dmin;improved = true;
        }
      }
    }
    return improved;
  }
  
  private boolean nominalFixedDecline(double[] exps, double[] qis)
  {
    boolean improved = false;
    int lmonth = exps.length;
    
    double transitionMonth = Double.MAX_VALUE;
    double dmin = this._minDecline;
    if (this._nominalDecline)
    {
      int l = 1;
      for (int lm1 = 0; l < exps.length; lm1++)
      {
        double decline = 2.0D * (exps[lm1] - exps[l]) / (exps[lm1] + exps[l]);
        if (decline < this._transitionSlope)
        {
          lmonth = lm1;
          transitionMonth = this._modelMonths[l] - 1.0D;
          if (!this._tailDecline.equals("Exponential")) {
            break;
          }
          dmin = Math.log(exps[lm1] / exps[l]); break;
        }
        l++;
      }
    }
    else
    {
      transitionMonth = this._transitionMonth - this._maxMonth - 1.0D;
      lmonth = (int)transitionMonth;
      if (lmonth > exps.length - 2) {
        lmonth = exps.length;
      } else if (this._tailDecline.equals("Exponential")) {
        dmin = Math.log(exps[lmonth] / exps[(lmonth + 1)]);
      }
    }
    if (lmonth < exps.length) {
      for (int l = lmonth + 1; l < exps.length; l++) {
        if (this._tailDecline.equals("Exponential")) {
          exps[lmonth] *= Math.exp((transitionMonth - this._modelMonths[l]) * dmin);
        } else if (this._tailDecline.equals("Minimum")) {
          exps[lmonth] *= (1.0D - (this._modelMonths[l] - transitionMonth) * this._minDecline);
        }
      }
    }
    for (int i = 0; i < qis.length; i++)
    {
      double sse = 0.0D;
      for (int l = 0; l < this._modelMonths.length; l++)
      {
        double q = qis[i] * exps[l];
        double err = this._modelProduction[l] - q;
        sse += err * err;
      }
      if (sse < this._sseMin)
      {
        this._sseMin = sse;this._mmin = lmonth;this._imin = i;improved = true;
      }
    }
    return improved;
  }
  
  private boolean naturalDecline(double[] exps, double[] qis)
  {
    boolean improved = false;
    for (int i = 0; i < qis.length; i++)
    {
      double sse = 0.0D;
      for (int l = 0; l < this._modelMonths.length; l++)
      {
        double q = qis[i] * exps[l];
        double err = this._modelProduction[l] - q;
        sse += err * err;
      }
      if (sse < this._sseMin)
      {
        this._sseMin = sse;this._imin = i;improved = true;
      }
    }
    return improved;
  }
  
  public DCAmodel copy()
  {
    DCAmodel copy = null;
    if ((this instanceof DCAarps)) {
      copy = new DCAarps();
    }
    if ((this instanceof DCAsepd)) {
      copy = new DCAsepd();
    }
    if ((this instanceof DCAduong)) {
      copy = new DCAduong();
    }
    if ((this instanceof DCAlgm)) {
      copy = new DCAlgm();
    }
    if ((this instanceof DCApowerLaw)) {
      copy = new DCApowerLaw();
    }
    copy.setProduction(getProduction(), this._ioUnits);
    copy.setModeledProduction(this._modelProduction);
    copy.setOutliers(this._outliers);
    copy.setMaxMonth(this._maxMonth);
    copy.setModel(toIO(this._dcaParm1), this._dcaParm2, this._dcaParm3, this._dcaParm4);
    copy.setHiExtremes(this._dcaHi);
    copy.setLoExtremes(this._dcaLo);
    return copy;
  }
  
  public double getSecantDecline()
  {
    double initial = model(this._maxMonth);
    double secant = (initial - model(this._maxMonth + 12)) / initial;
    return secant;
  }
  
  public double getFitCC()
  {
    if ((this._modelMonths == null) || (this._modelProduction == null)) {
      return NaN.0D;
    }
    double[] modeled = new double[this._modelProduction.length];
    double[] predict = new double[this._modelProduction.length];
    for (int i = 0; i < this._modelProduction.length; i++)
    {
      modeled[i] = toIO(this._modelProduction[i]);
      predict[i] = toIO(model(this._modelMonths[i] + this._maxMonth - 1.0D));
    }
    return DCAUtil.correlationCoefficient(modeled, predict);
  }
  
  public double getFitRC()
  {
    if ((this._modelMonths == null) || (this._modelProduction == null)) {
      return NaN.0D;
    }
    double[] modeled = new double[this._modelProduction.length];
    double[] predict = new double[this._modelProduction.length];
    for (int i = 0; i < this._modelProduction.length; i++)
    {
      modeled[i] = this._modelProduction[i];
      predict[i] = model(this._modelMonths[i] + this._maxMonth - 1.0D);
    }
    return DCAUtil.correlationCoefficient(
      DCAUtil.rank(modeled), DCAUtil.rank(predict));
  }
  
  protected void defaultModelExtremes() {}
}
