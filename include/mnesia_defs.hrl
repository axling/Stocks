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


-record(market, {tax_factor = 0.3,
		 courtage = 10,
		 holdings = [],
		 min = 1000,
		 money = 5000,
		 risk = 0.5,
		 stop_loss,
		 portfolio_value=[],
		 history = []}).

-record(optimize, {
	  market=#market{},
	  type=simulated_annealing,
	  criteria,
	  retries=10,
	  sa_temperature=10000,
	  sa_reduction=0.9,
	  sa_iterations=100,
	  ga_mutation=85,
	  ga_generations=100,
	  cu_limit=20000
	 }).
