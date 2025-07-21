open Safe
open Pack

let accbands = Conv.indicator_to_safe (Indicator.accbands_upper ())
let acos = Conv.indicator_to_safe (Indicator.acos ())
let ad = Conv.indicator_to_safe (Indicator.ad ())
let add = Conv.indicator_to_safe (Indicator.add ())
let adosc = Conv.indicator_to_safe (Indicator.adosc ())
let adx = Conv.indicator_to_safe (Indicator.adx ())
let adxr = Conv.indicator_to_safe (Indicator.adxr ())
let apo = Conv.indicator_to_safe (Indicator.apo ())
let aroon = Pack (Aroon { timeperiod = 14 })
let aroonosc = Conv.indicator_to_safe (Indicator.aroon_osc ())
let asin = Conv.indicator_to_safe (Indicator.asin ())
let atan = Conv.indicator_to_safe (Indicator.atan ())
let atr = Conv.indicator_to_safe (Indicator.atr ())
let avgprice = Conv.indicator_to_safe (Indicator.avg_price ())
let avgdev = Conv.indicator_to_safe (Indicator.avgdev ())
let bbands =
  Pack
    (Bbands
       {
         timeperiod = 20;
         nb_dev_up = 2.0;
         nb_dev_dn = 2.0;
         ma_type = Ma_type.Sma;
       })
let beta = Conv.indicator_to_safe (Indicator.beta ())
let bop = Conv.indicator_to_safe (Indicator.bop ())
let cci = Conv.indicator_to_safe (Indicator.cci ())
let cdl2crows = Conv.indicator_to_safe (Indicator.cdl_2crows ())
let cdl3blackcrows = Conv.indicator_to_safe (Indicator.cdl_3blackcrows ())
let cdl3inside = Conv.indicator_to_safe (Indicator.cdl_3inside ())
let cdl3linestrike = Conv.indicator_to_safe (Indicator.cdl_3linestrike ())
let cdl3outside = Conv.indicator_to_safe (Indicator.cdl_3outside ())
let cdl3starsinsouth = Conv.indicator_to_safe (Indicator.cdl_3starsinsouth ())
let cdl3whitesoldiers = Conv.indicator_to_safe (Indicator.cdl_3whitesoldiers ())
let cdlabandonedbaby = Conv.indicator_to_safe (Indicator.cdl_abandonedbaby ())
let cdladvanceblock = Conv.indicator_to_safe (Indicator.cdl_advanceblock ())
let cdlbelthold = Conv.indicator_to_safe (Indicator.cdl_belthold ())
let cdlbreakaway = Conv.indicator_to_safe (Indicator.cdl_breakaway ())
let cdlclosingmarubozu = Conv.indicator_to_safe (Indicator.cdl_closingmarubozu ())
let cdlconcealbabyswall = Conv.indicator_to_safe (Indicator.cdl_concealbabyswall ())
let cdlcounterattack = Conv.indicator_to_safe (Indicator.cdl_counterattack ())
let cdldarkcloudcover = Conv.indicator_to_safe (Indicator.cdl_darkcloudcover ())
let cdldoji = Conv.indicator_to_safe (Indicator.cdl_doji ())
let cdldojistar = Conv.indicator_to_safe (Indicator.cdl_dojistar ())
let cdldragonflydoji = Conv.indicator_to_safe (Indicator.cdl_dragonflydoji ())
let cdlengulfing = Conv.indicator_to_safe (Indicator.cdl_engulfing ())
let cdleveningdojistar = Conv.indicator_to_safe (Indicator.cdl_eveningdojistar ())
let cdleveningstar = Conv.indicator_to_safe (Indicator.cdl_eveningstar ())
let cdlgapsidesidewhite = Conv.indicator_to_safe (Indicator.cdl_gap_side_side_white ())
let cdlgravestonedoji = Conv.indicator_to_safe (Indicator.cdl_gravestonedoji ())
let cdlhammer = Conv.indicator_to_safe (Indicator.cdl_hammer ())
let cdlhangingman = Conv.indicator_to_safe (Indicator.cdl_hangingman ())
let cdlharami = Conv.indicator_to_safe (Indicator.cdl_harami ())
let cdlharamicross = Conv.indicator_to_safe (Indicator.cdl_haramicross ())
let cdlhighwave = Conv.indicator_to_safe (Indicator.cdl_highwave ())
let cdlhikkake = Conv.indicator_to_safe (Indicator.cdl_hikkake ())
let cdlhikkakemod = Conv.indicator_to_safe (Indicator.cdl_hikkakemod ())
let cdlhomingpigeon = Conv.indicator_to_safe (Indicator.cdl_homingpigeon ())
let cdlidentical3crows = Conv.indicator_to_safe (Indicator.cdl_identical3crows ())
let cdlinneck = Conv.indicator_to_safe (Indicator.cdl_inneck ())
let cdlinvertedhammer = Conv.indicator_to_safe (Indicator.cdl_invertedhammer ())
let cdlkicking = Conv.indicator_to_safe (Indicator.cdl_kicking ())
let cdlkickingbylength = Conv.indicator_to_safe (Indicator.cdl_kickingbylength ())
let cdlladderbottom = Conv.indicator_to_safe (Indicator.cdl_ladderbottom ())
let cdllongleggeddoji = Conv.indicator_to_safe (Indicator.cdl_longleggedDoji ())
let cdllongline = Conv.indicator_to_safe (Indicator.cdl_longline ())
let cdlmarubozu = Conv.indicator_to_safe (Indicator.cdl_marubozu ())
let cdlmatchinglow = Conv.indicator_to_safe (Indicator.cdl_matchinglow ())
let cdlmathold = Conv.indicator_to_safe (Indicator.cdl_mathold ())
let cdlmorningdojistar = Conv.indicator_to_safe (Indicator.cdl_morningdojistar ())
let cdlmorningstar = Conv.indicator_to_safe (Indicator.cdl_morningstar ())
let cdlonneck = Conv.indicator_to_safe (Indicator.cdl_onneck ())
let cdlpiercing = Conv.indicator_to_safe (Indicator.cdl_piercing ())
let cdlrickshawman = Conv.indicator_to_safe (Indicator.cdl_rickshawman ())
let cdlrisefall3methods = Conv.indicator_to_safe (Indicator.cdl_risefall3methods ())
let cdlseparatinglines = Conv.indicator_to_safe (Indicator.cdl_separatinglines ())
let cdlshootingstar = Conv.indicator_to_safe (Indicator.cdl_shootingstar ())
let cdlshortline = Conv.indicator_to_safe (Indicator.cdl_shortline ())
let cdlspinningtop = Conv.indicator_to_safe (Indicator.cdl_spinningtop ())
let cdlstalledpattern = Conv.indicator_to_safe (Indicator.cdl_stalledpattern ())
let cdlsticksandwich = Conv.indicator_to_safe (Indicator.cdl_sticksandwich ())
let cdltakuri = Conv.indicator_to_safe (Indicator.cdl_takuri ())
let cdltasukigap = Conv.indicator_to_safe (Indicator.cdl_tasukigap ())
let cdlthrusting = Conv.indicator_to_safe (Indicator.cdl_thrusting ())
let cdltristar = Conv.indicator_to_safe (Indicator.cdl_tristar ())
let cdlunique3river = Conv.indicator_to_safe (Indicator.cdl_unique3river ())
let cdlupsidegap2crows = Conv.indicator_to_safe (Indicator.cdl_upsidegap2crows ())
let cdlxsidegap3methods = Conv.indicator_to_safe (Indicator.cdl_xsidegap3methods ())
let ceil = Conv.indicator_to_safe (Indicator.ceil ())
let cmo = Conv.indicator_to_safe (Indicator.cmo ())
let correl = Conv.indicator_to_safe (Indicator.correl ())
let cos = Conv.indicator_to_safe (Indicator.cos ())
let cosh = Conv.indicator_to_safe (Indicator.cosh ())
let dema = Conv.indicator_to_safe (Indicator.dema ())
let div = Conv.indicator_to_safe (Indicator.div ())
let dx = Conv.indicator_to_safe (Indicator.dx ())
let ema = Conv.indicator_to_safe (Indicator.ema ())
let exp = Conv.indicator_to_safe (Indicator.exp ())
let floor = Conv.indicator_to_safe (Indicator.floor ())
let ht_dcperiod = Conv.indicator_to_safe (Indicator.ht_dc_period ())
let ht_dcphase = Conv.indicator_to_safe (Indicator.ht_dc_phase ())
let ht_phasor = Pack (Ht_phasor ())
let ht_sine = Pack (Ht_sine ())
let ht_trendline = Conv.indicator_to_safe (Indicator.ht_trendline ())
let ht_trendmode = Conv.indicator_to_safe (Indicator.ht_trend_mode ())
let imi = Conv.indicator_to_safe (Indicator.imi ())
let kama = Conv.indicator_to_safe (Indicator.kama ())
let linearreg = Conv.indicator_to_safe (Indicator.linearreg ())
let linearreg_angle = Conv.indicator_to_safe (Indicator.linearreg_angle ())
let linearreg_intercept = Conv.indicator_to_safe (Indicator.linearreg_intercept ())
let linearreg_slope = Conv.indicator_to_safe (Indicator.linearreg_slope ())
let ln = Conv.indicator_to_safe (Indicator.ln ())
let log10 = Conv.indicator_to_safe (Indicator.log10 ())
let ma = Conv.indicator_to_safe (Indicator.ma ())
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
let mavp = Conv.indicator_to_safe (Indicator.mavp ())
let max = Conv.indicator_to_safe (Indicator.max ())
let maxindex = Conv.indicator_to_safe (Indicator.max_index ())
let medprice = Conv.indicator_to_safe (Indicator.med_price ())
let mfi = Conv.indicator_to_safe (Indicator.mfi ())
let midpoint = Conv.indicator_to_safe (Indicator.midpoint ())
let midprice = Conv.indicator_to_safe (Indicator.midprice ())
let min = Conv.indicator_to_safe (Indicator.min ())
let minindex = Conv.indicator_to_safe (Indicator.min_index ())
let minmax = Pack (Minmax { timeperiod = 14 })
let minmaxindex = Pack (Minmaxindex { timeperiod = 14 })
let minus_di = Conv.indicator_to_safe (Indicator.minus_di ())
let minus_dm = Conv.indicator_to_safe (Indicator.minus_dm ())
let mom = Conv.indicator_to_safe (Indicator.mom ())
let mult = Conv.indicator_to_safe (Indicator.mult ())
let natr = Conv.indicator_to_safe (Indicator.natr ())
let obv = Conv.indicator_to_safe (Indicator.obv ())
let plus_di = Conv.indicator_to_safe (Indicator.plus_di ())
let plus_dm = Conv.indicator_to_safe (Indicator.plus_dm ())
let ppo = Conv.indicator_to_safe (Indicator.ppo ())
let roc = Conv.indicator_to_safe (Indicator.roc ())
let rocp = Conv.indicator_to_safe (Indicator.rocp ())
let rocr = Conv.indicator_to_safe (Indicator.rocr ())
let rocr100 = Conv.indicator_to_safe (Indicator.rocr100 ())
let rsi = Conv.indicator_to_safe (Indicator.rsi ())
let sar = Conv.indicator_to_safe (Indicator.sar ())
let sarext = Conv.indicator_to_safe (Indicator.sarext ())
let sin = Conv.indicator_to_safe (Indicator.sin ())
let sinh = Conv.indicator_to_safe (Indicator.sinh ())
let sma = Conv.indicator_to_safe (Indicator.sma ())
let sqrt = Conv.indicator_to_safe (Indicator.sqrt ())
let stddev = Conv.indicator_to_safe (Indicator.stddev ())
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
let sub = Conv.indicator_to_safe (Indicator.sub ())
let sum = Conv.indicator_to_safe (Indicator.sum ())
let t3 = Conv.indicator_to_safe (Indicator.t3 ())
let tan = Conv.indicator_to_safe (Indicator.tan ())
let tanh = Conv.indicator_to_safe (Indicator.tanh ())
let tema = Conv.indicator_to_safe (Indicator.tema ())
let trange = Conv.indicator_to_safe (Indicator.trange ())
let trima = Conv.indicator_to_safe (Indicator.trima ())
let trix = Conv.indicator_to_safe (Indicator.trix ())
let tsf = Conv.indicator_to_safe (Indicator.tsf ())
let typprice = Conv.indicator_to_safe (Indicator.typ_price ())
let ultosc = Conv.indicator_to_safe (Indicator.ultosc ())
let var = Conv.indicator_to_safe (Indicator.var ())
let wclprice = Conv.indicator_to_safe (Indicator.wcl_price ())
let willr = Conv.indicator_to_safe (Indicator.willr ())
let wma = Conv.indicator_to_safe (Indicator.wma ())

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
