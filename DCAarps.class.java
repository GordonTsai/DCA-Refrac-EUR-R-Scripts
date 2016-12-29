package com.drillinginfo.dca;

public class DCAarps
  extends DCAmodel
{
  public DCAarps()
  {
    this._type = "Arps";
    this._LMfunction = new LMarps();
    
    this._dcaParm1 = 30000.0D;
    this._dcaParm2 = 20.0D;
    this._dcaParm3 = 1.5D;
    defaultModelExtremes();
  }
  
  protected void searchInitial()
    throws IllegalStateException
  {
    boolean refine = true;int nattempts = 0;
    while (refine)
    {
      double q0 = 0.0D;double di = 0.0D;double b = 0.0D;
      this._sseMin = Double.MAX_VALUE;
      
      double pmax = this._production[(this._maxMonth - 1)];
      
      int[] nsteps = { 40, 20 };int iter = nsteps.length;
      double div = 1.0D / (nsteps[0] - 1);
      
      double qimin = Math.max(this._dcaLo[0], pmax / 2.0D);
      double qimax = Math.min(this._dcaHi[0], 2.0D * pmax);
      double dq = div * (qimax - qimin);
      
      double dimin = this._dcaLo[1];
      double dimax = this._dcaHi[1];
      double dd = div * (dimax - dimin);
      
      double bmin = this._dcaLo[2];
      double bmax = this._dcaHi[2];
      double db = div * (bmax - bmin);
      for (int it = 0; it < iter; it++)
      {
        int imin = 0;int jmin = 0;int kmin = 0;
        double[] qis = new double[nsteps[it]];
        double[] bs = new double[nsteps[it]];
        for (int i = 0; i < nsteps[it]; i++)
        {
          qis[i] = (qimin + i * dq);
          bs[i] = (bmin + i * db);
        }
        for (int j = 0; j < nsteps[it]; j++)
        {
          double diMonth = dimin + j * dd;
          for (int k = 0; k < bs.length; k++)
          {
            double dib = diMonth * bs[k];
            double nib = -1.0D / bs[k];
            double[] pow = new double[this._modelMonths.length];
            for (int l = 0; l < this._modelMonths.length; l++) {
              pow[l] = Math.pow(1.0D + dib * (this._modelMonths[l] - 1.0D), nib);
            }
            for (int i = 0; i < qis.length; i++)
            {
              double sse = 0.0D;
              for (int l = 0; l < this._modelMonths.length; l++)
              {
                double q = qis[i] * pow[l];
                double err = this._modelProduction[l] - q;
                sse += err * err;
              }
              if (sse < this._sseMin)
              {
                this._sseMin = sse;imin = i;jmin = j;kmin = k;
              }
            }
          }
        }
        if (it < iter - 1)
        {
          div = 1.0D / (nsteps[(it + 1)] - 1);
          
          q0 = qimin + imin * dq;
          qimin = Math.max(q0 - dq, qimin);
          qimax = Math.min(q0 + dq, qimax);
          dq = div * (qimax - qimin);
          
          di = dimin + jmin * dd;
          dimin = Math.max(di - dd, dimin);
          dimax = Math.min(di + dd, dimax);
          dd = div * (dimax - dimin);
          
          b = bmin + kmin * db;
          bmin = Math.max(b - db, bmin);
          bmax = Math.min(b + db, bmax);
          db = div * (bmax - bmin);
        }
        else
        {
          q0 = qimin + imin * dq;
          di = dimin + jmin * dd;
          b = bmin + kmin * db;
        }
      }
      this._dcaParm1 = q0;
      this._dcaParm2 = di;
      this._dcaParm3 = b;
      
      refine = false;
      if ((this._dcaParm2 == this._dcaLo[1]) && (this._dcaParm3 == this._dcaLo[2]))
      {
        this._dcaHi[1] /= 2.0D;this._dcaHi[2] /= 2.0D;
        refine = true;
      }
      if (refine) {
        nattempts++;
      }
      if (nattempts == 5) {
        throw new IllegalStateException("Cannot converge in initial search");
      }
    }
  }
  
  protected void search()
  {
    double q0 = 0.0D;double di = 0.0D;double b = 0.0D;
    this._sseMin = Double.MAX_VALUE;
    
    int[] nsteps = { 20, 10 };int iter = nsteps.length;
    double div = 1.0D / (nsteps[0] - 1);
    
    double qimin = Math.max(this._dcaLo[0], this._dcaParm1 / 2.0D);
    double qimax = Math.min(this._dcaHi[0], 2.0D * this._dcaParm1);
    double dq = div * (qimax - qimin);
    
    double dimin = Math.max(this._dcaLo[1], this._dcaParm2 / 2.0D);
    double dimax = Math.min(this._dcaHi[1], this._dcaParm2 * 2.0D);
    double dd = div * (dimax - dimin);
    
    double bmin = Math.max(this._dcaLo[2], this._dcaParm3 / 2.0D);
    double bmax = Math.min(this._dcaHi[2], this._dcaParm3 * 2.0D);
    double db = div * (bmax - bmin);
    for (int it = 0; it < iter; it++)
    {
      this._imin = (this._jmin = this._kmin = 0);
      double[] qis = new double[nsteps[it]];
      double[] bs = new double[nsteps[it]];
      for (int i = 0; i < nsteps[it]; i++)
      {
        qis[i] = (qimin + i * dq);
        bs[i] = (bmin + i * db);
      }
      for (int j = 0; j < nsteps[it]; j++)
      {
        double diMonth = dimin + j * dd;
        for (int k = 0; k < nsteps[it]; k++)
        {
          double dib = diMonth * bs[k];
          double nib = -1.0D / bs[k];
          double[] pow = new double[this._modelMonths.length];
          for (int l = 0; l < this._modelMonths.length; l++) {
            pow[l] = Math.pow(1.0D + dib * (this._modelMonths[l] - 1.0D), nib);
          }
          if (terminalDecline(pow, qis))
          {
            this._jmin = j;this._kmin = k;
          }
        }
      }
      if (it < iter - 1)
      {
        div = 1.0D / (nsteps[(it + 1)] - 1);
        
        q0 = qimin + this._imin * dq;
        qimin = Math.max(q0 - dq, qimin);
        qimax = Math.min(q0 + dq, qimax);
        dq = div * (qimax - qimin);
        
        di = dimin + this._jmin * dd;
        dimin = Math.max(di - dd, dimin);
        dimax = Math.min(di + dd, dimax);
        dd = div * (dimax - dimin);
        
        b = bmin + this._kmin * db;
        bmin = Math.max(b - db, bmin);
        bmax = Math.min(b + db, bmax);
        db = div * (bmax - bmin);
      }
      else
      {
        q0 = qimin + this._imin * dq;
        di = dimin + this._jmin * dd;
        b = bmin + this._kmin * db;
        if (!this._tailDecline.equals("Natural"))
        {
          if ((this._searchDecline) && (this._mmin != 0)) {
            this._transitionMonth = (this._modelMonths[this._mmin] + this._maxMonth);
          }
          if ((this._nominalDecline) && 
            (this._mmin < this._modelMonths.length)) {
            this._transitionMonth = (this._modelMonths[this._mmin] + this._maxMonth - 1.0D);
          }
        }
      }
    }
    this._dcaParm1 = q0;
    this._dcaParm2 = di;
    this._dcaParm3 = b;
    this._dcaParm4 = this._transitionMonth;
    
    double dib = di * b;
    double nib = -1.0D / b;
    double t = this._transitionMonth - this._maxMonth;
    double q1 = Math.pow(1.0D + dib * (t - 0.5D), nib);
    double q2 = Math.pow(1.0D + dib * (t + 0.5D), nib);
    this._transitionSlope = (2.0D * (q1 - q2) / (q1 + q2));
    if (this._tailDecline.equals("Exponential")) {
      this._minDecline = Math.log(q1 / q2);
    }
  }
  
  public double bestQi(double di, double b)
  {
    double qi = 0.0D;double nib = -1.0D / b;double dib = di * b;
    double sseMin = Double.MAX_VALUE;
    
    double[] production = regressionProduction();
    double[] cumulative = null;
    if (this._useCumulative) {
      cumulative = regressionCumProduction();
    }
    double pmax = this._production[(this._maxMonth - 1)];
    
    int[] nsteps = { 40, 20 };int iter = nsteps.length;
    double div = 1.0D / (nsteps[0] - 1);
    
    double qimin = Math.max(this._dcaLo[0], pmax / 2.0D);
    double qimax = Math.min(this._dcaHi[0], 2.0D * pmax);
    double dq = div * (qimax - qimin);
    
    double[] pow = new double[this._modelMonths.length];
    for (int l = 0; l < this._modelMonths.length; l++) {
      pow[l] = Math.pow(1.0D + dib * (this._modelMonths[l] - 1.0D), nib);
    }
    for (int it = 0; it < iter; it++)
    {
      int imin = 0;
      double[] qis = new double[nsteps[it]];
      for (int i = 0; i < nsteps[it]; i++) {
        qis[i] = (qimin + i * dq);
      }
      for (int i = 0; i < nsteps[it]; i++)
      {
        double sse = 0.0D;double Q = 0.0D;
        for (int l = 0; l < this._modelMonths.length; l++)
        {
          double q = qis[i] * pow[l];
          if (this._useProduction)
          {
            double err = regressionError(production[l], q);
            sse += err * err;
          }
          if (this._useCumulative)
          {
            Q += q;
            double cerr = regressionError(cumulative[l], Q);
            sse += cerr * cerr;
          }
        }
        if (sse < sseMin)
        {
          sseMin = sse;imin = i;
        }
      }
      if (it < iter - 1)
      {
        div = 1.0D / (nsteps[(it + 1)] - 1);
        
        qi = qimin + imin * dq;
        qimin = Math.max(qi - dq, qimin);
        qimax = Math.min(qi + dq, qimax);
        dq = div * (qimax - qimin);
      }
      else
      {
        qi = qimin + imin * dq;
      }
    }
    return toIO(qi);
  }
  
  public double model(double q0, double di, double b, double d, double t)
  {
    if (t < 1.0D) {
      return 0.0D;
    }
    double nib = -1.0D / b;
    if (t < this._maxMonth) {
      return this._q0 + t / this._maxMonth * (q0 - this._q0);
    }
    double db = di * b;
    double tt = t - this._maxMonth;
    double transitionMonth = this._transitionMonth - this._maxMonth - 1.0D;
    if ((this._tailDecline.equals("Natural")) || (tt < transitionMonth)) {
      return q0 * Math.pow(1.0D + db * tt, nib);
    }
    double dmin = this._minDecline;
    double q1 = Math.pow(1.0D + db * transitionMonth, nib);
    if (this._tailDecline.equals("Exponential"))
    {
      double p1 = Math.pow(1.0D + db * (transitionMonth - 0.5D), nib);
      double p2 = Math.pow(1.0D + db * (transitionMonth + 0.5D), nib);
      dmin = Math.log(p1 / p2);
      dmin = Math.exp((transitionMonth - tt) * dmin);
    }
    else if (this._tailDecline.equals("Minimum"))
    {
      dmin = 1.0D - (tt - transitionMonth) * this._minDecline;
    }
    return q0 * q1 * dmin;
  }
  
  public String report(boolean reserves, double cumulative)
  {
    String report = "Arps' Equation Model\n";
    report = report + "    Qi: " + toIO(this._dcaParm1) + " (" + this._ioUnits + ")\n";
    report = report + "    Di: " + this._dcaParm2 + "\n";
    report = report + "     b: " + this._dcaParm3 + "\n";
    if (!this._tailDecline.equals("Natural"))
    {
      report = report + "    di: " + this._transitionSlope + "\n";
      report = report + "    ti: " + this._transitionMonth + "\n";
    }
    report = report + super.report(reserves, cumulative);
    return report;
  }
  
  public String[] getNames()
  {
    return new String[] { "Qi", "Di", "b" };
  }
  
  protected void defaultModelExtremes()
  {
    this._dcaLo = new double[] { 1.0D, 1.0E-6D, 1.0E-4D, 6.0D };
    this._dcaHi = new double[] { 120000.0D, 0.8D, 2.0D, 1200.0D };
  }
}
