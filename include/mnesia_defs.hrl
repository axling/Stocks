-record(stocks, {company, date, highest, lowest, closing, average, turnover, volume, completions}).

-record(company, {name, instrument, market}).

-record(analysis, {company, date, type, result}).

-record(trend_result, {date, days, slope, trend}).

-record(adx, {company, date, value, di_plus, di_minus}).

-record(macd, {company, date, value, signal}).

-record(atr, {company, date, value}).

-record(mvg_avg, {company, date, ten, thirty}).

-record(exp_avg, {company, date, ten, thirty}).



