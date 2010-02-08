-record(stocks, {company, date, highest, lowest, closing, average, turnover, volume, completions}).

-record(company, {name, instrument, market}).

-record(analysis, {company, date, type, result}).

-record(trend_result, {date, days, slope, trend}).

