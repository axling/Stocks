-record(stocks, {company, date, highest, lowest, closing, average, turnover, volume, completions}).

-record(stock, {date, highest, lowest, closing, average, turnover, volume, completions}).

-record(sec, {name, instrument, market, data, day_trend=0, week_trend=0, month_trend=0, year_trend=0}).

-record(company, {name, instrument, market}).

-record(analysis, {company, type, result}).

-record(adx, {date, value, di_plus, di_minus}).

-record(macd, {date, value, signal}).

-record(atr, {date, value}).

-record(mvg_avg, {date, ten, thirty}).

-record(exp_avg, {date, ten, thirty}).

-record(stochastic, {date, percent_k, percent_d}).

-record(task, {id, name, type, time, action}).


