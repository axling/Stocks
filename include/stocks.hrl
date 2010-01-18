-record(market, {tax_factor = 0.3,
		 courtage = 10,
		 holdings = [],
		 min = 1000,
		 money = 5000,
		 risk = 0.5,
		 history = []}).

-record(stock, {buy, sell, name, turnover, highest, lowest}).
