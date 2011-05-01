%%%-------------------------------------------------------------------
%%% File    : http_lib.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 20 Sep 2009 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(http_lib).

-export([download/1, download_stock_data/3, download_stock_data2/3]).

%%Side-effects
download(Page) ->
    inets:start(),
    {ok, {_,Headers, Body}} = httpc:request(Page),
    inets:stop(),
    {Headers, Body}.

download_stock_data(Instrument, StartDate, EndDate) ->
    StartDateString = date_lib:convert_date_e_s(StartDate),
    EndDateString = date_lib:convert_date_e_s(EndDate),   
    
    Request = 
	"<post>\n"
	++ "<param name=\"SubSystem\" value=\"History\"/>\n"
	++ "<param name=\"Action\" value=\"GetDataSeries\"/>\n"
	++ "<param name=\"AppendIntraDay\" value=\"no\"/>\n"
	++ "<param name=\"Instrument\" value=\"" ++ Instrument ++ "\"/>\n"
	++ "<param name=\"FromDate\" value=\"" ++ StartDateString ++ "\"/>\n"
	++ "<param name=\"ToDate\" value=\"" ++ EndDateString ++ "\"/>\n"
	++ "<param name=\"hi__a\" value=\"0,1,2,4,21,8,10,11,12,9\"/>\n"
	++ "<param name=\"ext_xslt\" value=\"test/hi_table.xsl\"/>\n"
	++ "<param name=\"ext_xslt_options\" value=\",undefined,\"/>\n"
	++ "<param name=\"ext_xslt_lang\" value=\"sv\"/>\n"
	++ "<param name=\"ext_xslt_hiddenattrs\" value=\",ip,iv,\"/>\n"
	++ "<param name=\"ext_xslt_tableId\" value=\"historicalTable\"/>\n"
	++ "</post>",
    
    UrlEncodedReq = edoc_lib:escape_uri(Request),
    {ok, {_, _, Result}} = httpc:request(post, {"http://www.nasdaqomxnordic.com/webproxy/DataFeedProxy.aspx",
					       [], "application/x-www-form-urlencoded;charset=UTF-8", 
					       "xmlquery=" ++ UrlEncodedReq}, 
					[], []),
    Result.

download_stock_data2(Instrument, StartDate, EndDate) ->
    StartDateString = date_lib:convert_date_e_s(StartDate),
    EndDateString = date_lib:convert_date_e_s(EndDate),   
    
    Request = 
	"<post>\n"
	++ "<param name=\"SubSystem\" value=\"History\"/>\n"
	++ "<param name=\"Action\" value=\"GetDataSeries\"/>\n"
	++ "<param name=\"AppendIntraDay\" value=\"no\"/>\n"
	++ "<param name=\"Instrument\" value=\"" ++ Instrument ++ "\"/>\n"
	++ "<param name=\"FromDate\" value=\"" ++ StartDateString ++ "\"/>\n"
	++ "<param name=\"ToDate\" value=\"" ++ EndDateString ++ "\"/>\n"
	++ "<param name=\"hi__a\" value=\"0,1,2,4,21,8,10,11,12,9\"/>\n"
	++ "<param name=\"ext_xslt\" value=\"test/hi_table.xsl\"/>\n"
	++ "<param name=\"ext_xslt_options\" value=\",undefined,\"/>\n"
	++ "<param name=\"ext_xslt_lang\" value=\"sv\"/>\n"
	++ "<param name=\"ext_xslt_hiddenattrs\" value=\",ip,iv,\"/>\n"
	++ "<param name=\"ext_xslt_tableId\" value=\"historicalTable\"/>\n"
	++ "</post>",
    
    UrlEncodedReq = edoc_lib:escape_uri(Request),
    {ok, _, _, Result} =  ibrowse:send_req("http://www.nasdaqomxnordic.com/webproxy/DataFeedProxy.aspx",
		     [{"Content-Type", "application/x-www-form-urlencoded;charset=UTF-8"}], 
		     post, "xmlquery=" ++ UrlEncodedReq,
		     [{proxy_host, "www-proxy.ericsson.se"}, {proxy_port, 8080}]),
    Result.

	
	
