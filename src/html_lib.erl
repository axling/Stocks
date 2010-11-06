%%%-------------------------------------------------------------------
%%% File    : html_lib.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 23 Sep 2009 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(html_lib).

-include("stocks.hrl").
%%-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
%%-export([parse_page/1]).

-define(GENERAL_SPAN, "<span\\s*class=\"[YCW]\">[^<]+</span>").
%%-define(GENERAL_SPAN, "\n").
-define(FLOAT, "[0-9]+\\.[0-9]+").
-define(NUMBER, "(?:" ++ ?FLOAT ++ "|\\d+)").
-define(NAME, "\\S+").
-define(STOCK_TYPE1, "<span\\s*class=\"[YC]\">\\s*(?<diff>" ++ ?NUMBER ++
	")\\s+(?<buy1>" ++ ?NUMBER ++ ")\\s+(?<sell1>" ++ ?NUMBER ++ 
	")\\s+(?<name1>" ++ ?NAME ++ ")\\s+(?<latest1>" ++ ?NUMBER ++ 
	")\\s+(?<number1>" ++ ?NUMBER ++ ")\\s*</span>").
-define(STOCK_TYPE2, "<span\\s*class=\"W\">\\s*(?<buy2>" ++ 
	?NUMBER ++")\\s+(?<sell2>" ++ ?NUMBER ++ 
	")\\s+(?<name2>" ++ ?NAME ++ ")\\s+(?<latest2>" ++ ?NUMBER ++ 
	")\\s+(?<number3>" ++ ?NUMBER ++ ")\\s*</span>").
-define(STOCK_TYPE3, "<span\\s*class=\"[YC]\">\\s*" ++ ?NUMBER ++
	"\\s+" ++ ?NUMBER ++ "\\s+" ++ ?NUMBER ++ 
	"\\s+(?<name3>" ++ ?NAME ++ ")\\s+" ++ ?NUMBER ++ 
	"\\s+(?<highest3>" ++ ?NUMBER ++ ")\\s+(?<lowest3>" ++ 
	?NUMBER ++ ")\\s*</span>").
-define(STOCK_TYPE4, "<span\\s*class=\"W\">\\s*"
	++ ?NUMBER ++ "\\s+" ++ ?NUMBER ++ 
	"\\s+(?<name4>" ++ ?NAME ++ ")\\s+" ++ ?NUMBER ++ 
	"\\s+(?<highest4>" ++ ?NUMBER ++ ")\\s+(?<lowest4>" ++ 
	?NUMBER ++ ")\\s*</span>").

-define(STOCKS, "(" ++ ?STOCK_TYPE1 ++ "|" ++ ?STOCK_TYPE2 ++ "|" 
	++ ?STOCK_TYPE3 ++ "|" ++ ?STOCK_TYPE4 ++ ")").

is_match(String, RegExp) ->
    case re:run(String, RegExp) of
	nomatch ->
	    false;
	_Else ->
	    true
    end.

print() ->
    io:format("Regexp1: ~p~nRegexp2: ~p~nRegexp3: ~p~nRegexp4: ~p~n",
	      [?STOCK_TYPE1,?STOCK_TYPE2,?STOCK_TYPE3,?STOCK_TYPE4]).

match(String, RegExp) ->
    re:run(String, RegExp, [{capture, all_but_first, list}]).

split(String, RegExp) ->
    re:split(String, RegExp,[{return, list}, trim]).

parse_page(Page) ->
    StringList = split(Page, "\n"),
    NewStringList = lists:filter(
 		      fun(String) ->
 			      is_match(String,?STOCKS)
 		      end, StringList),
    ResultList = 
	lists:map(
	  fun(String) ->
		  case match_type1(String) of
		      {error, Reason} ->
			  {error, Reason};
		      #stock{} = Stock ->
			  Stock
		  end
	  end, NewStringList),
    merge_entrys(ResultList).


match_type1(String) ->
    case match(String, ?STOCK_TYPE1) of
	{match, [_Diff, Buy, Sell, Name, _Latest, Number]} ->
	    #stock{buy=list_to_number(Buy), sell=list_to_number(Sell), 
		   name=Name, turnover=list_to_integer(Number)};
	nomatch ->
	    match_type2(String)
    end.

match_type2(String) ->
    case match(String, ?STOCK_TYPE2) of
	{match, [Buy, Sell, Name, _Latest, Number]} ->
	    #stock{buy=list_to_number(Buy), sell=list_to_number(Sell), 
		   name=Name, turnover=list_to_integer(Number)};
	nomatch ->
	    match_type3(String)
    end.

match_type3(String) ->
    case match(String, ?STOCK_TYPE3) of
	{match, [Name, Highest, Lowest]} ->
	    #stock{name=Name, highest=list_to_number(Highest), lowest=list_to_number(Lowest)};
	nomatch ->
	    match_type4(String)
    end.

match_type4(String) ->
    case match(String, ?STOCK_TYPE4) of
	{match, [Name, Highest, Lowest]} ->
	    #stock{name=Name, highest=list_to_number(Highest), lowest=list_to_number(Lowest)};
	nomatch ->
	    match_type4(String)
    end.

merge_entrys(ResultList) ->
    NewList = 
	lists:foldl(
	  fun(#stock{name=Name} = Entry, AccIn) ->
		  case lists:keyfind(Name, 1, AccIn) of
		      {Name, List} ->
			  lists:keyreplace(Name, 1, AccIn, {Name, [Entry | List]});
		      false ->
			  [{Name, [Entry]} | AccIn]
		  end
	  end, [], ResultList),    
    lists:map(fun({_Name, [Stock1, Stock2]}) -> 
		      merge_record(Stock1, Stock2, size(Stock1))
	      end, NewList).

merge_record(Stock1, _Stock2, 0) ->
    Stock1;
merge_record(Stock1, Stock2, Index) ->
    if 
	element(Index, Stock1) == element(Index, Stock2) ->
	    merge_record(Stock1, Stock2, Index - 1);
	(element(Index, Stock1) == undefined) and 
	(element(Index, Stock2) /= undefined) ->
	    NewStock1 = setelement(Index, Stock1, element(Index, Stock2)),
	    merge_record(NewStock1, Stock2, Index - 1);
	true ->
	    merge_record(Stock1, Stock2, Index - 1)
    end.

list_to_number(Number) ->	
    case catch list_to_float(Number) of
	{'EXIT',_} ->
	    list_to_integer(Number);
	Else ->
	    Else
    end.


%%-----------------------------------------------------------
%% EUNIT Tests
%%-----------------------------------------------------------
%% compile_regexp_test_() ->
%%     [?_assertMatch({ok, _}, re:compile(?STOCK_TYPE1)),
%%      ?_assertMatch({ok, _}, re:compile(?STOCK_TYPE2)),
%%      ?_assertMatch({ok, _}, re:compile(?STOCK_TYPE3)),
%%      ?_assertMatch({ok, _}, re:compile(?STOCK_TYPE4)),
%%      ?_assertMatch({ok, _}, re:compile(?STOCKS))].

%% is_match_test_() ->
%%     [?_assert(is_match("<span class=\"Y\">  3 148.3 148.5 ABB   148.5     2574350</span>", ?GENERAL_SPAN)),
%%      ?_assert(is_match("<span class=\"Y\">  3 148.3 148.5 ABB   148.5     2574350</span>", ?STOCK_TYPE1)),
%%      ?_assert(is_match("<span class=\"W\">     64.8    65 CAST   64.8      996998</span>", ?STOCK_TYPE2)),
%%      ?_assert(is_match("<span class=\"Y\">  3 148.3 148.5 ABB   148.5 149.3 146.3</span>", ?STOCK_TYPE3)),
%%      ?_assert(is_match("<span class=\"W\">     64.8    65 CAST   64.8    65  64.3</span>", ?STOCK_TYPE4))
%%     ].

%% match_test_() ->
%%     [?_assertMatch({match, ["3", "148.3", "148.5", "ABB", "148.5", "2574350"]}, match("<span class=\"Y\">  3 148.3 148.5 ABB   148.5     2574350</span>", ?STOCK_TYPE1)),
%%      ?_assertMatch({match, ["64.8", "65", "CAST", "64.8", "996998"]}, match("<span class=\"W\">     64.8    65 CAST   64.8      996998</span>", ?STOCK_TYPE2)),
%%      ?_assertMatch({match, ["ABB", "149.3", "146.3"]}, match("<span class=\"Y\">  3 148.3 148.5 ABB   148.5 149.3 146.3</span>", ?STOCK_TYPE3)),
%%      ?_assertMatch({match, ["CAST", "65", "64.3"]}, match("<span class=\"W\">     64.8    65 CAST   64.8    65  64.3</span>", ?STOCK_TYPE4))].

-define(PARSE_PAGE_RESULT, [{stock,127.5,127.8,"GETIb",1248331,128.3,126},
			    {stock,42.4,42.5,"FABG",1288324,42.7,42.1},                              
			    {stock,71.2,71.3,"ERICb",26504616,71.8,70.1},                            
			    {stock,70.7,71,"ERICa",81434,71.2,69.6},                                  
			    {stock,170.3,170.5,"ELUXb",2497691,172,167.5},
			    {stock,167.3,173.5,"ELUXa",1101,171.8,167},
			    {stock,137.5,137.8,"EKTAb",341460,138.8,136.3},
			    {stock,66.3,66.5,"CAST",877833,67.3,65.3},
			    {stock,86.7,86.8,"BOL",11411632,86.8,82.5},
			    {stock,308,308.5,"AZN",1923496,311.5,307.5},
			    {stock,196,196.5,"AXFO",84893,197.5,194.5},
			    {stock,85.3,85.4,"ATCOb",1322286,85.5,83.2},
			    {stock,96,96.1,"ATCOa",6994739,96.3,93.8},
			    {stock,118.1,118.2,"ASSAb",2503972,119.9,117.5},
			    {stock,103,103.3,"AOIL",1501641,106.5,103},
			    {stock,249.5,250,"ALIV",707535,250,241},
			    {stock,84,84.1,"ALFA",2351595,84.7,83.5},
			    {stock,151,151.5,"ABB",3320106,152,147.3}]).

-define(PAGE, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><title>SVT Text - 203 </title><meta http-equiv=\"Content-type\" content=\"text/html; charset=iso-8859-1\" /><meta http-equiv=\"Content-language\" content=\"sv\" /><meta name=\"Author\" content=\"Sveriges Television AB, Stockholm, Sweden\" /><meta name=\"Copyright\" content=\"Sveriges Television AB, Stockholm, Sweden\" /><meta name=\"Robots\" content=\"index, follow, noarchive\" /><meta name=\"Description\" content=\"SVTs Text-TV p\345 internet. Nyheter, Ekonomi, Sport, M\345lservice 377, V\344der, TV... \" /><link href=\"../../css/svttextstyle.css\" rel=\"stylesheet\" type=\"text/css\" /><link href=\"../locals/localstyle.css\" rel=\"stylesheet\" type=\"text/css\" /><script language=\"JavaScript\" type=\"text/javascript\"><!--var nextPage = \"202.html\";var previousPage = \"204.html\";// --></script><script language=\"JavaScript\" src=\"../../script/svttextscript.js\" type=\"text/javascript\"></script><script language=\"JavaScript\" src=\"../locals/localscript.js\" type=\"text/javascript\"></script></head><body onload=\"setFocus('navform','pageinput');\" bgcolor=\"#FFFFFF\"><a name=\"AnchorTop\"></a><div id=\"logo\"><a href=\"100.html\"><img src=\"../../images/logoSvtText.gif\" width=\"146\" height=\"26\" alt=\"SVT Text\" border=\"0\" /></a></div><div id=\"wrapper\"><div id=\"topLine\"></div><ul id=\"menu\"><li><span class=\"mpNumber\"><a href=\"100.html\" class=\"mpNumber\"> 100</a></span><a href=\"100.html\">Nyheter</a></li><li><span class=\"mpNumber\"><a href=\"200.html\" class=\"mpNumber\"> 200</a></span><a href=\"200.html\">Ekonomi</a></li><li><span class=\"mpNumber\"><a href=\"300.html\" class=\"mpNumber\"> 300</a></span><a href=\"300.html\">Sport</a></li><li><span class=\"mpNumber\"><a href=\"400.html\" class=\"mpNumber\"> 400</a></span><a href=\"400.html\">V\344der</a></li><li><span class=\"mpNumber\"><a href=\"500.html\" class=\"mpNumber\"> 500</a></span><a href=\"500.html\">Blandat</a></li><li><span class=\"mpNumber\"><a href=\"600.html\" class=\"mpNumber\"> 600</a></span><a href=\"600.html\">P\345 TV</a></li><li><span class=\"mpNumber\"><a href=\"700.html\" class=\"mpNumber\"> 700</a></span><a href=\"700.html\">Inneh\345ll</a></li><li><span class=\"mpNumber\"><a href=\"800.html\" class=\"mpNumber\"> 800</a></span><a href=\"800.html\">UR</a></li><li id=\"help\"><a href=\"javascript:SgOpenArgs('http://svt.se/svt/jsp/Crosslink.jsp?d=50238','texttvhelp','550','500','status=yes,scrollbars=yes');\">Hj&auml;lp</a> <img src=\"../../images/iconHelp.gif\" width=\"13\" height=\"13\" alt=\"\" border=\"0\" align=\"middle\" /></li></ul><div id=\"topNav\"><span class=\"leftSetting\">Utseende: <a href=\"javascript:settingsNavigate(webLookFolder,203);\" class=\"webView\" title=\"Visa sidan med webbutseende\">Webb</a> | <a href=\"javascript:settingsNavigate(tvLookFolder,203);\" class=\"tvView\" title=\"Visa sidan med TV-utseende\">TV</a></span><div class=\"centerNav\"><form title=\"Navigering till sidnummer\" id=\"navform\" name=\"navform\" action=\"jsp/gotopage.jsp\" onsubmit=\"return formNavigate(this);\"><a href=\"202.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnBack.gif\" width=\"31\" height=\"18\" alt=\"F\366reg\345ende sida\" border=\"0\" /></a>&nbsp;<input id=\"pageinput\" name=\"pageinput\" type=\"text\" maxlength=\"3\"  title=\"Ange \366nskat sidnummer\" value=\"203\" />&nbsp;<span class=\"btnBg\"><input id=\"submitButton\" onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" type=\"image\" src=\"../../images/btnGoToPage.gif\" title=\"G\345 till sida\" value=\"G\345 till sida\" alt=\"G\345 till sida\" /></span>&nbsp;<a href=\"204.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnForward.gif\" width=\"31\" height=\"18\" alt=\"N\344sta sida\" border=\"0\" /></a></form></div><span class=\"sizeSetting\"><a href=\"javascript:settingsNavigate(normalSizeFolder,203);\" class=\"aNormal\">normal</a> | <a href=\"javascript:settingsNavigate(largeSizeFolder,203);\" class=\"aLarger\">st\366rre</a> | <a href=\"javascript:settingsNavigate(xlargeSizeFolder,203);\" class=\"aLargest\">st\366rst</a></span></div><?xml version=\"1.0\" encoding=\"UTF-8\"?><div><a class=\"preclass\" name=\"subpage1\"> </a><pre class=\"root\"> 203 SVT Text        Torsdag 15 okt 2009\n <span class=\"W\">K\344lla: SIX Telekurs  091014            </span>\n <span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"B bgY DH\">LARGE CAP: ABB-GETI       </span><span class=\"R bgY DH\">SLUTKURSER</span>\n <span class=\"Y\">+ </span><span class=\"C\">- </span><span class=\"W\">  K\326P  S\304LJ NAMN SENAST       ANTAL</span>\n <span class=\"G\">                                       </span>\n <span class=\"Y\">5.3   151 151.5 ABB   151.5     3320106</span>\n <span class=\"Y\">1.1    84  84.1 ALFA   84.1     2351595</span>\n <span class=\"Y\">9.5 249.5   250 ALIV  249.5      707535</span>\n <span class=\"C\">0.3   103 103.3 AOIL    103     1501641</span>\n <span class=\"Y\">0.9 118.1 118.2 ASSAb 118.1     2503972</span>\n <span class=\"Y\">3.5    96  96.1 ATCOa  96.1     6994739</span>\n <span class=\"Y\">3.2  85.3  85.4 ATCOb  85.4     1322286</span>\n <span class=\"Y\">  3   196 196.5 AXFO  196.5       84893</span>\n <span class=\"W\">      308 308.5 AZN   308.5     1923496</span>\n <span class=\"Y\">5.8  86.7  86.8 BOL    86.8    11411632</span>\n <span class=\"Y\">1.8  66.3  66.5 CAST   66.5      877833</span>\n <span class=\"Y\">1.3 137.5 137.8 EKTAb 137.8      341460</span>\n <span class=\"Y\">1.3 167.3 173.5 ELUXa   167        1101</span>\n <span class=\"Y\">4.8 170.3 170.5 ELUXb 170.5     2497691</span>\n <span class=\"Y\">1.2  70.7    71 ERICa  70.8       81434</span>\n <span class=\"Y\">1.1  71.2  71.3 ERICb  71.2    26504616</span>\n <span class=\"Y\">0.5  42.4  42.5 FABG   42.4     1288324</span>\n <span class=\"Y\">2.3 127.5 127.8 GETIb 127.8     1248331</span>\n</pre><a class=\"preclass\" name=\"subpage2\"> </a><pre class=\"root sub\"> 203 SVT Text        Torsdag 15 okt 2009\n <span class=\"W\">K\344lla: SIX Telekurs  091014            </span>\n <span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"B bgY DH\">LARGE CAP: ABB-GETI       </span><span class=\"R bgY DH\">SLUTKURSER</span>\n <span class=\"Y\">+ </span><span class=\"C\">- </span><span class=\"W\">  K\326P  S\304LJ NAMN SENAST H\326GST L\304GST</span>\n <span class=\"G\">                                       </span>\n <span class=\"Y\">5.3   151 151.5 ABB   151.5   152 147.3</span>\n <span class=\"Y\">1.1    84  84.1 ALFA   84.1  84.7  83.5</span>\n <span class=\"Y\">9.5 249.5   250 ALIV  249.5   250   241</span>\n <span class=\"C\">0.3   103 103.3 AOIL    103 106.5   103</span>\n <span class=\"Y\">0.9 118.1 118.2 ASSAb 118.1 119.9 117.5</span>\n <span class=\"Y\">3.5    96  96.1 ATCOa  96.1  96.3  93.8</span>\n <span class=\"Y\">3.2  85.3  85.4 ATCOb  85.4  85.5  83.2</span>\n <span class=\"Y\">  3   196 196.5 AXFO  196.5 197.5 194.5</span>\n <span class=\"W\">      308 308.5 AZN   308.5 311.5 307.5</span>\n <span class=\"Y\">5.8  86.7  86.8 BOL    86.8  86.8  82.5</span>\n <span class=\"Y\">1.8  66.3  66.5 CAST   66.5  67.3  65.3</span>\n <span class=\"Y\">1.3 137.5 137.8 EKTAb 137.8 138.8 136.3</span>\n <span class=\"Y\">1.3 167.3 173.5 ELUXa   167 171.8   167</span>\n <span class=\"Y\">4.8 170.3 170.5 ELUXb 170.5   172 167.5</span>\n <span class=\"Y\">1.2  70.7    71 ERICa  70.8  71.2  69.6</span>\n <span class=\"Y\">1.1  71.2  71.3 ERICb  71.2  71.8  70.1</span>\n <span class=\"Y\">0.5  42.4  42.5 FABG   42.4  42.7  42.1</span>\n <span class=\"Y\">2.3 127.5 127.8 GETIb 127.8 128.3   126</span>\n</pre></div><div class=\"subWrapper\"><div class=\"subArea\"></div></div><div class=\"clear\" id=\"footerWrapper\"><div id=\"bottomNav\"><span class=\"leftSetting\">Automatisk uppdatering: <a href=\"javascript:settingsNavigate(updateOffFolder,203);\" class=\"updateOff\">Av</a> | <a href=\"javascript:settingsNavigate(updateOnFolder,203);\" class=\"updateOn\">P\345</a></span><div class=\"centerNav\"><a href=\"202.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnBack.gif\" width=\"31\" height=\"18\" alt=\"F\366reg\345ende sida\" border=\"0\" /></a>&nbsp;<span class=\"upArrow\"><a href=\"#AnchorTop\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnUp.gif\" width=\"19\" height=\"18\" alt=\"\" border=\"0\" /></a></span>&nbsp;<a href=\"204.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnForward.gif\" width=\"31\" height=\"18\" alt=\"N\344sta sida\" border=\"0\" /></a></div></div></div></div><div class=\"clear subWrapper\"><div class=\"copyArea\"><p>&copy; Sveriges Television AB | Anja Hild\351n |  <a href=\"mailto:text@svt.se\">text@svt.se</a></p></div></div><!-- Begin Sitestat code --><script language=JavaScript1.1 type=text/javascript>sitestat(\"http://ld.svt.se/svt/svt/s?svt-text.Ekonomi.203&client=svttext\");</script><noscript><img src=\"http://ld.svt.se/svt/svt/s?svt-text.Ekonomi.203&client=svttext\" width=\"1\" height=\"1\" alt=\"\"></noscript><!-- End Sitestat code --></body></html>").

%% parse_page_test() ->
%%     ?assertMatch(?PARSE_PAGE_RESULT, parse_page(?PAGE)).


