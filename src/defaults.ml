open Safe
open Pack

let accbands = Pack (Accbands { timeperiod = 20 })
let acos = Pack (Acos ())
let ad = Pack (Ad ())
let add = Pack (Add ())
let adosc = Pack (Adosc { fast_period = 12; slow_period = 26 })
let adx = Pack (Adx { timeperiod = 14 })
let adxr = Pack (Adxr { timeperiod = 14 })

let apo =
  Pack (Apo { fast_period = 12; slow_period = 26; ma_type = Ma_type.Ema })

let aroon = Pack (Aroon { timeperiod = 14 })
let aroonosc = Pack (Aroonosc { timeperiod = 14 })
let asin = Pack (Asin ())
let atan = Pack (Atan ())
let atr = Pack (Atr { timeperiod = 14 })
let avgprice = Pack (Avgprice ())
let avgdev = Pack (Avgdev { timeperiod = 14 })

let bbands =
  Pack
    (Bbands
       {
         timeperiod = 20;
         nb_dev_up = 2.0;
         nb_dev_dn = 2.0;
         ma_type = Ma_type.Sma;
       })

let beta = Pack (Beta { timeperiod = 14 })
let bop = Pack (Bop ())
let cci = Pack (Cci { timeperiod = 14 })
let cdl2crows = Pack (Cdl2crows ())
let cdl3blackcrows = Pack (Cdl3blackcrows ())
let cdl3inside = Pack (Cdl3inside ())
let cdl3linestrike = Pack (Cdl3linestrike ())
let cdl3outside = Pack (Cdl3outside ())
let cdl3starsinsouth = Pack (Cdl3starsinsouth ())
let cdl3whitesoldiers = Pack (Cdl3whitesoldiers ())
let cdlabandonedbaby = Pack (Cdlabandonedbaby { penetration = 0.3 })
let cdladvanceblock = Pack (Cdladvanceblock ())
let cdlbelthold = Pack (Cdlbelthold ())
let cdlbreakaway = Pack (Cdlbreakaway ())
let cdlclosingmarubozu = Pack (Cdlclosingmarubozu ())
let cdlconcealbabyswall = Pack (Cdlconcealbabyswall ())
let cdlcounterattack = Pack (Cdlcounterattack ())
let cdldarkcloudcover = Pack (Cdldarkcloudcover { penetration = 0.3 })
let cdldoji = Pack (Cdldoji ())
let cdldojistar = Pack (Cdldojistar ())
let cdldragonflydoji = Pack (Cdldragonflydoji ())
let cdlengulfing = Pack (Cdlengulfing ())
let cdleveningdojistar = Pack (Cdleveningdojistar { penetration = 0.3 })
let cdleveningstar = Pack (Cdleveningstar { penetration = 0.3 })
let cdlgapsidesidewhite = Pack (Cdlgapsidesidewhite ())
let cdlgravestonedoji = Pack (Cdlgravestonedoji ())
let cdlhammer = Pack (Cdlhammer ())
let cdlhangingman = Pack (Cdlhangingman ())
let cdlharami = Pack (Cdlharami ())
let cdlharamicross = Pack (Cdlharamicross ())
let cdlhighwave = Pack (Cdlhighwave ())
let cdlhikkake = Pack (Cdlhikkake ())
let cdlhikkakemod = Pack (Cdlhikkakemod ())
let cdlhomingpigeon = Pack (Cdlhomingpigeon ())
let cdlidentical3crows = Pack (Cdlidentical3crows ())
let cdlinneck = Pack (Cdlinneck ())
let cdlinvertedhammer = Pack (Cdlinvertedhammer ())
let cdlkicking = Pack (Cdlkicking ())
let cdlkickingbylength = Pack (Cdlkickingbylength ())
let cdlladderbottom = Pack (Cdlladderbottom ())
let cdllongleggeddoji = Pack (Cdllongleggeddoji ())
let cdllongline = Pack (Cdllongline ())
let cdlmarubozu = Pack (Cdlmarubozu ())
let cdlmatchinglow = Pack (Cdlmatchinglow ())
let cdlmathold = Pack (Cdlmathold { penetration = 0.3 })
let cdlmorningdojistar = Pack (Cdlmorningdojistar { penetration = 0.3 })
let cdlmorningstar = Pack (Cdlmorningstar { penetration = 0.3 })
let cdlonneck = Pack (Cdlonneck ())
let cdlpiercing = Pack (Cdlpiercing ())
let cdlrickshawman = Pack (Cdlrickshawman ())
let cdlrisefall3methods = Pack (Cdlrisefall3methods ())
let cdlseparatinglines = Pack (Cdlseparatinglines ())
let cdlshootingstar = Pack (Cdlshootingstar ())
let cdlshortline = Pack (Cdlshortline ())
let cdlspinningtop = Pack (Cdlspinningtop ())
let cdlstalledpattern = Pack (Cdlstalledpattern ())
let cdlsticksandwich = Pack (Cdlsticksandwich ())
let cdltakuri = Pack (Cdltakuri ())
let cdltasukigap = Pack (Cdltasukigap ())
let cdlthrusting = Pack (Cdlthrusting ())
let cdltristar = Pack (Cdltristar ())
let cdlunique3river = Pack (Cdlunique3river ())
let cdlupsidegap2crows = Pack (Cdlupsidegap2crows ())
let cdlxsidegap3methods = Pack (Cdlxsidegap3methods ())
let ceil = Pack (Ceil ())
let cmo = Pack (Cmo { timeperiod = 14 })
let correl = Pack (Correl { timeperiod = 14 })
let cos = Pack (Cos ())
let cosh = Pack (Cosh ())
let dema = Pack (Dema { timeperiod = 14 })
let div = Pack (Div ())
let dx = Pack (Dx { timeperiod = 14 })
let ema = Pack (Ema { timeperiod = 14 })
let exp = Pack (Exp ())
let floor = Pack (Floor ())
let ht_dcperiod = Pack (Ht_dcperiod ())
let ht_dcphase = Pack (Ht_dcphase ())
let ht_phasor = Pack (Ht_phasor ())
let ht_sine = Pack (Ht_sine ())
let ht_trendline = Pack (Ht_trendline ())
let ht_trendmode = Pack (Ht_trendmode ())
let imi = Pack (Imi { timeperiod = 14 })
let kama = Pack (Kama { timeperiod = 14 })
let linearreg = Pack (Linearreg { timeperiod = 14 })
let linearreg_angle = Pack (Linearreg_angle { timeperiod = 14 })
let linearreg_intercept = Pack (Linearreg_intercept { timeperiod = 14 })
let linearreg_slope = Pack (Linearreg_slope { timeperiod = 14 })
let ln = Pack (Ln ())
let log10 = Pack (Log10 ())
let ma = Pack (Ma { timeperiod = 14; ma_type = Ma_type.Ema })
let macd = Pack (Macd { fast_period = 12; slow_period = 26; signal_period = 9 })

let macdext =
  Pack
    (Macdext
       {
         fast_period = 12;
         fast_ma_type = Ma_type.Ema;
         slow_period = 26;
         slow_ma_type = Ma_type.Ema;
         signal_period = 9;
         signal_ma_type = Ma_type.Ema;
       })

let macdfix = Pack (Macdfix { signal_period = 9 })
let mama = Pack (Mama { fast_limit = 0.5; slow_limit = 0.05 })

let mavp =
  Pack (Mavp { min_period = 2; max_period = 30; ma_type = Ma_type.Ema })

let max = Pack (Max { timeperiod = 14 })
let maxindex = Pack (Maxindex { timeperiod = 14 })
let medprice = Pack (Medprice ())
let mfi = Pack (Mfi { timeperiod = 14 })
let midpoint = Pack (Midpoint { timeperiod = 14 })
let midprice = Pack (Midprice { timeperiod = 14 })
let min = Pack (Min { timeperiod = 14 })
let minindex = Pack (Minindex { timeperiod = 14 })
let minmax = Pack (Minmax { timeperiod = 14 })
let minmaxindex = Pack (Minmaxindex { timeperiod = 14 })
let minus_di = Pack (Minus_di { timeperiod = 14 })
let minus_dm = Pack (Minus_dm { timeperiod = 14 })
let mom = Pack (Mom { timeperiod = 10 })
let mult = Pack (Mult ())
let natr = Pack (Natr { timeperiod = 14 })
let obv = Pack (Obv ())
let plus_di = Pack (Plus_di { timeperiod = 14 })
let plus_dm = Pack (Plus_dm { timeperiod = 14 })

let ppo =
  Pack (Ppo { fast_period = 12; slow_period = 26; ma_type = Ma_type.Ema })

let roc = Pack (Roc { timeperiod = 10 })
let rocp = Pack (Rocp { timeperiod = 10 })
let rocr = Pack (Rocr { timeperiod = 10 })
let rocr100 = Pack (Rocr100 { timeperiod = 10 })
let rsi = Pack (Rsi { timeperiod = 14 })
let sar = Pack (Sar { acceleration = 0.02; maximum = 0.2 })

let sarext =
  Pack
    (Sarext
       {
         start_value = 0.0;
         offset_on_reverse = 0.0;
         acceleration_init_long = 0.02;
         acceleration_long = 0.02;
         acceleration_max_long = 0.2;
         acceleration_init_short = 0.02;
         acceleration_short = 0.02;
         acceleration_max_short = 0.2;
       })

let sin = Pack (Sin ())
let sinh = Pack (Sinh ())
let sma = Pack (Sma { timeperiod = 14 })
let sqrt = Pack (Sqrt ())
let stddev = Pack (Stddev { timeperiod = 14; nb_dev = 1.0 })

let stoch =
  Pack
    (Stoch
       {
         fast_k_period = 5;
         slow_k_period = 3;
         slow_k_ma_type = Ma_type.Sma;
         slow_d_period = 3;
         slow_d_ma_type = Ma_type.Sma;
       })

let stochf =
  Pack
    (Stochf
       { fast_k_period = 5; fast_d_period = 3; fast_d_ma_type = Ma_type.Sma })

let stochrsi =
  Pack
    (Stochrsi
       {
         timeperiod = 14;
         fast_k_period = 5;
         fast_d_period = 3;
         fast_d_ma_type = Ma_type.Ema;
       })

let sub = Pack (Sub ())
let sum = Pack (Sum { timeperiod = 14 })
let t3 = Pack (T3 { timeperiod = 14; v_factor = 0.7 })
let tan = Pack (Tan ())
let tanh = Pack (Tanh ())
let tema = Pack (Tema { timeperiod = 14 })
let trange = Pack (Trange ())
let trima = Pack (Trima { timeperiod = 14 })
let trix = Pack (Trix { timeperiod = 14 })
let tsf = Pack (Tsf { timeperiod = 14 })
let typprice = Pack (Typprice ())

let ultosc =
  Pack (Ultosc { timeperiod1 = 7; timeperiod2 = 14; timeperiod3 = 28 })

let var = Pack (Var { timeperiod = 14; nb_dev = 1.0 })
let wclprice = Pack (Wclprice ())
let willr = Pack (Willr { timeperiod = 14 })
let wma = Pack (Wma { timeperiod = 14 })

let all =
  [
    accbands;
    acos;
    ad;
    add;
    adosc;
    adx;
    adxr;
    apo;
    aroon;
    aroonosc;
    asin;
    atan;
    atr;
    avgprice;
    avgdev;
    bbands;
    beta;
    bop;
    cci;
    cdl2crows;
    cdl3blackcrows;
    cdl3inside;
    cdl3linestrike;
    cdl3outside;
    cdl3starsinsouth;
    cdl3whitesoldiers;
    cdlabandonedbaby;
    cdladvanceblock;
    cdlbelthold;
    cdlbreakaway;
    cdlclosingmarubozu;
    cdlconcealbabyswall;
    cdlcounterattack;
    cdldarkcloudcover;
    cdldoji;
    cdldojistar;
    cdldragonflydoji;
    cdlengulfing;
    cdleveningdojistar;
    cdleveningstar;
    cdlgapsidesidewhite;
    cdlgravestonedoji;
    cdlhammer;
    cdlhangingman;
    cdlharami;
    cdlharamicross;
    cdlhighwave;
    cdlhikkake;
    cdlhikkakemod;
    cdlhomingpigeon;
    cdlidentical3crows;
    cdlinneck;
    cdlinvertedhammer;
    cdlkicking;
    cdlkickingbylength;
    cdlladderbottom;
    cdllongleggeddoji;
    cdllongline;
    cdlmarubozu;
    cdlmatchinglow;
    cdlmathold;
    cdlmorningdojistar;
    cdlmorningstar;
    cdlonneck;
    cdlpiercing;
    cdlrickshawman;
    cdlrisefall3methods;
    cdlseparatinglines;
    cdlshootingstar;
    cdlshortline;
    cdlspinningtop;
    cdlstalledpattern;
    cdlsticksandwich;
    cdltakuri;
    cdltasukigap;
    cdlthrusting;
    cdltristar;
    cdlunique3river;
    cdlupsidegap2crows;
    cdlxsidegap3methods;
    ceil;
    cmo;
    correl;
    cos;
    cosh;
    dema;
    div;
    dx;
    ema;
    exp;
    floor;
    ht_dcperiod;
    ht_dcphase;
    ht_phasor;
    ht_sine;
    ht_trendline;
    ht_trendmode;
    imi;
    kama;
    linearreg;
    linearreg_angle;
    linearreg_intercept;
    linearreg_slope;
    ln;
    log10;
    ma;
    macd;
    macdext;
    macdfix;
    mama;
    mavp;
    max;
    maxindex;
    medprice;
    mfi;
    midpoint;
    midprice;
    min;
    minindex;
    minmax;
    minmaxindex;
    minus_di;
    minus_dm;
    mom;
    mult;
    natr;
    obv;
    plus_di;
    plus_dm;
    ppo;
    roc;
    rocp;
    rocr;
    rocr100;
    rsi;
    sar;
    sarext;
    sin;
    sinh;
    sma;
    sqrt;
    stddev;
    stoch;
    stochf;
    stochrsi;
    sub;
    sum;
    t3;
    tan;
    tanh;
    tema;
    trange;
    trima;
    trix;
    tsf;
    typprice;
    ultosc;
    var;
    wclprice;
    willr;
    wma;
  ]

let common_indicators =
  [
    accbands;
    ad;
    adosc;
    adx;
    adxr;
    apo;
    aroon;
    aroonosc;
    atr;
    avgprice;
    bbands;
    bop;
    cci;
    cdlengulfing;
    cdldoji;
    cdlhammer;
    cdlharami;
    cdlmorningstar;
    cdlshootingstar;
    cdlspinningtop;
    cmo;
    dema;
    dx;
    ema;
    ht_trendline;
    imi;
    kama;
    ma;
    macd;
    mfi;
    mom;
    natr;
    obv;
    plus_di;
    plus_dm;
    ppo;
    roc;
    rsi;
    sar;
    sma;
    stoch;
    stochf;
    stochrsi;
    tema;
    trange;
    trix;
    ultosc;
    willr;
    wma;
  ]
