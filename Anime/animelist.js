/*
 *  File:  	/C/Notes/Anime/animelist.js
 *  Purpose:	Javascript definitions for HTML lists of Anime shows
 *  See also:	/C/Notes/Anime/animelist.css
 *  By:		Chris Chittleborough, June 2019
 *	Edited Jan 2020: Second param is now any alternate name(s), not just Japanese
 *	Edited Jul 2020: now accepts WHEN values like "Sat AM"
 *
 *  This file 'exports' one function and several constants.
 *  (Names beginning with '_' are for internal use only.)
 *
 *  The function is:
 *    add_show (
 *	name,		// S	my name for show
 *	onames,		// S?	other name(s*), if relevant; else ""
 *	myrating,	// I	rNoNoNo, rNoNo, … rSoSo, rYes … rToTryForSure
 *	mycomment,	// S	_short_ personal reaction; max width is 8ex
 *	site,		// I	CrRoll … Netflix or UnknownSite
 *	sitepath,	// S	(Tail of) URL for show at site, or "X" if URL unknown
 *	when,		// I	"Sun 00:00", "Sat 23:59", NotWeekly, "2021-05", etc
 *	ANNEnum,	// I	# of entry in ANN's encyclopedia or 0
 *	tvt,		// S?	Tail of URL of TV Tropes article, or ""
 *	wp,		// S?	Tail of URL of Wikipedia article, or ""
 *	tags,		// [S]	short descriptors for the show; must have at least one
 *	desc )		// S??	longer description; can use <i>, <em>, etc.
 *  where
 *    ONAMES may be of the form "<abbrev-name><TAB><full-name>", and
 *    WHEN is usually like "Thu 01:30" but can be any of these:
 *		NotWeekly	= not weekly (eg., already completed)
 *		"?"		= weekly, day and time unknown
 *		"Sun?"		= weekly, on that day in some time-zone, time unknown
 *		"Mon ??:??"	= weekly, on that day, time of day unknown
 *		"Tue AM"	= weekly, on that day, early morning
 *		"Wed 23:45"	= weekly, on that day and at that time
 *		"2021-05"	= batch (eg., Netflix) by year and month
 */

"use strict";

// TO-DO LATER: sorting table by columns (up or down), filtering rows
// Note:  Array.sort() is stable in Gecko 1.9 and later

// 'Exported' constants for Ratings:
const rNoNoNo=111, rNoNo=112, rNo=113, rSoSo=114,
      rNone=115, rYes=116, rYesYes=117, rYesYesYes=118,
      rToTryMaybePerhaps=119, rToTryMaybe=120, rToTryLikely=121, rToTryForSure=122;
// Private constants for ratings:
var _RATING_INFO = {};
const _rClasses=0, _rText=1, _rTitle=2;
_RATING_INFO[rNoNoNo]		= [ "rNo3",	 "✘✘✘",	"Will never watch"	];
_RATING_INFO[rNoNo]		= [ "rNo2",	 "✘✘",  "Unlikely to watch"	];
_RATING_INFO[rNo]		= [ "rNo1",	 "✘",   "No plans to watch"	];
_RATING_INFO[rSoSo]		= [ "rSoSo",	 "⁓",	"Probably boring"	];
_RATING_INFO[rNone]		= [ "rNone", "?\u00A0?\u00A0?", "Not enough info yet" ];
_RATING_INFO[rYes]		= [ "rYes1",	 "✔",	"Maybe watch one day?"	];
_RATING_INFO[rYesYes]		= [ "rYes2",	 "✔✔",	"Probably watch"	];
_RATING_INFO[rYesYesYes]	= [ "rYes3",	 "✔✔✔",	"Will watch for sure"	];
_RATING_INFO[rToTryMaybePerhaps]= [ "ToTry rSoSo", "(⁓)",   "Try iff desparate"];
_RATING_INFO[rToTryMaybe]	= [ "ToTry rYes1", "(✓)",   "Could try, maybe"	];
_RATING_INFO[rToTryLikely]	= [ "ToTry rYes2", "(✓✓)",  "Probably try"	];
_RATING_INFO[rToTryForSure]	= [ "ToTry rYes3", "(✓✓✓)", "Definitely try"	];
Object.freeze(_RATING_INFO);

// 'Exported' constants for websites:
const CrRoll=1001,  AnLab=1002,   Funimation=1003, HiDive=1004, AmazonP=1005,
      Netflix=1006, Youtube=1007, Disney=1008, AdultSwim=1009, AniPlus=1010,
      UnknownSite=1023;
// 'Private' constants for websites:
var _SITE_INFO = {};
const _siteName=0, _siteAbbrev=1, _siteBase=2;
_SITE_INFO[CrRoll]	= ["Crunchyroll", "Cr",	"https://www.crunchyroll.com/"];
_SITE_INFO[AnLab]	= ["Anime Lab",	  "AL",	"https://www.animelab.com/shows/"];
_SITE_INFO[Funimation]	= ["Funimation",  "F",	"https://www.funimation.com/shows/"];
_SITE_INFO[HiDive]	= ["HiDive",	  "Hd",	"https://www.hidive.com/tv/"];
_SITE_INFO[AmazonP]	= ["Amazon",	  "AP",	"https://www.amazon.com/dp/"];
_SITE_INFO[Netflix]	= ["Netflix",	  "Nf",	"https://www.netflix.com/au/title/"];
_SITE_INFO[Youtube]	= ["Youtube",	  "Yt", "https://youtube.com/"];
_SITE_INFO[Disney]	= ["Disney/Hulu", "D+", "https://disneyplus.com/"];
_SITE_INFO[AdultSwim]	= ["Adult Swim",  "AS", "https://www.adultswim.com/videos/"];
_SITE_INFO[AniPlus]	= ["AniPlus",	  "A+", "https://www.aniplus-asia.com/show/"];
_SITE_INFO[UnknownSite] = ["Website not known yet", "(Unknown)", ""];
Object.freeze(_SITE_INFO);

// Other 'exported' constants for calling add_show():
const NotWeekly={}, UnknownSchedule=NotWeekly, NoTvT={}, NoWP={};

// Other private constants:
var _anime_list_table;

// See top of file for documentation.
function add_show (
    name, onames,		// my name; japanese name(s) if relevant or else ""
    myrating, mycomment,	// rating (rYesYes etc); personal comment
    site, sitepath, when,	// AnLab etc; URL tail or "X"; "Thu 01:30" etc
    ANNEnum, tvt, wp,		// # in ANN encyclopedia; URL tails to TV Tropes & WP
    tags,			// Array of strings
    desc)			// Optional description
{
 try {

    //==== Internal constants and functions (put here to hide them from other files):

    const Required=true, Optional=false, EMDASH="—";
    const ANNE_base = "https://www.animenewsnetwork.com/encyclopedia/anime.php?id=",
	  TVT_base  = "https://tvtropes.org/pmwiki/pmwiki.php/",
	  WP_base   = "https://en.wikipedia.org/wiki/";

    // check_s -- throw if an argument is not a string, or is required but empty
    function check_s(v, arg_name, required) {
	if ( ! v instanceof String ) {
	    throw bad_arg(arg_name, v, 'is not a string');
	} else if ( required && ! v ) {
	    throw bad_arg(arg_name, v, 'should not be empty');
	}
    }
    // check_i -- throw unless an argument is a number
    //		If CHECK is Required, the number must also be non-zero;
    //		otherwise, the number is 'out of range' if CHECK is undefined.
    function check_i(v, arg_name, check) {
	if ( ! v instanceof Number ) {
	    throw bad_arg(arg_name, v, 'is not an number');
	} else if ( Math.floor(v) != v ) {
	    throw bad_arg(arg_name, v, 'is not a whole number');
	} else if ( check === undefined ) {
	    throw bad_arg(arg_name, v, 'is out of range');
	} else if ( v == 0 && check === Required ) {
	    throw bad_arg(arg_name, v, 'must be non-zero');
	}
    }
    // check_when -- check a WHEN value, throwing unless it is valid
    //		NotWeekly	→	null (a falsy value)
    //  	"?"		→	["???", "?"]
    //  	"Mon?"		→	["Mon?", "?"]
    //		"Tue ??:??"	→	["Tue", "?"]
    //  	"Wed 23:45"	→	["Wed", "23:45"]
    //		"2021-05"	→	["2021", "05"]
    function check_when(when) {
	const WHEN_RE =
	      /^(Sun|Mon|Tue|Wed|Thu|Fri|Sat)(\?| \?\?:\?\?| \d\d:\d\d| [AP]M)$/i;
	const WHEN_Y_M_RE = /^(20[2-9][0-9])-([01][0-9])$/
	if ( when == NotWeekly )
	    return null;
	check_s(when, "when", Required);
	if ( when == "?" )
	    return ["???", "?"];
	var match = WHEN_Y_M_RE.exec(when);
	if ( match ) {
	    return [match[1], match[2]]
	}
	match = WHEN_RE.exec(when);
	if (! match )
	    throw bad_arg("WHEN", when, 'should be "Wed 23:45", "Thu ??:??", "Fri?" etc');
	if ( match[2] == "?" )
	    return [match[1]+"?", "?"];
	var dow = match[1], tod = match[2].slice(1);
	if ( tod == "??:??" ) {
	    tod = "?";
	} else if ( tod == "AM" ) {
	    tod = "AM   "
	} else if ( tod == "PM" ) {
	    tod = "PM   "
	} else {
	    var hh = Number(tod.slice(0,2)), mm = Number(tod.slice(3));
	    if ( hh < 0 || hh > 23 || mm < 0 || mm > 59 )
		throw bad_arg("WHEN", when, 'has a bad time-of-day part');
	}
	return [dow, tod];
    }
    // bad_arg -- create an error report regarding an argument value
    function bad_arg(arg_name, v, problem) {
	return new Error('add_show("'+name+'"): bad argument:\n '+
			 arg_name + ' (value ' + v +') ' + problem);
    }
    // add_el -- add an element to the document's HTML
    //	parent	HTMLElement	New element will be the last child of this element
    //  tag	string		Kind of element to add
    //  text	string?		Optional text to attach to new element
    //	classes	string?		Optional className string for new element
    //  url	string?		Optional; if supplied, add_el() also adds an
    //				<a href=URL target=_blank> as the child of the
    //				new element and attaches any text to the <a>.
    function add_el(parent, tag, text, classes, url) {
	var el = document.createElement(tag);
	parent.appendChild(el);
	if ( classes ) {
	    el.className = classes;
	}
	var pot = el;	// Parent Of Text node
	if ( url ) {
	    pot = el.appendChild(document.createElement("A"));
	    pot.href = url;
	    pot.target = "_blank";
	    pot.rel = "noopener";
	}
	if ( text ) {
	    pot.appendChild(document.createTextNode(text));
	}
	return el;
    }

    //==== Check the arguments.
    var ri = _RATING_INFO[myrating], si	= _SITE_INFO[site], schedule;
    check_s(name,	"NAME",		Required);
    check_s(onames,	"ONAMES",	Optional);
    check_i(myrating,	"MYRATING",	ri);
    check_s(mycomment,	"MYCOMMENT",	Required);
    check_i(site,	"SITE",		si);
    check_s(sitepath,	"SITEPATH",	Required);
    schedule = check_when(when);
    check_i(ANNEnum,	"ANNEnum",	Optional);
    if ( tvt != NoTvT ) check_s(tvt, "TVT", Required);
    if ( tvt != NoWP )  check_s(wp,  "WP",  Required);
    if ( ! tags || ! (tags instanceof Array) )
	throw bad_arg("TAGS", tags, "is not an array");
    for ( let tag of tags ) {
	if ( ! tag instanceof String ) throw bad_arg("TAGS", tag, "is not a string");
    }
    if ( tags.length == 0 ) {
	throw bad_arg("TAGS", tags, "is empty; need at least one string");
    }
    if ( desc ) check_s(desc, "desc", Required);

    if ( ! _anime_list_table ) {
	_anime_list_table = document.querySelector("table#main");
	if ( ! _anime_list_table ) {
	    throw new Error("cannot find <table id=main ...>");
	}
    }

    //==== Add the first row for this show.
    var tr = _anime_list_table.insertRow(-1), td, e;
    tr.className="row1";
    //		First cell spans 4 cols, has name hyperlinked to ANN
    //		encyclopedia entry and perhaps a span.jname with Japanese name(s).
    td = add_el(tr, "td", "", "name-s");
    td.colSpan = 4;
    add_el(td, "a", name, "ANNElink", (ANNE_base + ANNEnum));
    if ( onames ) {
	var title = "", tabpos = onames.indexOf("\t");
	if ( tabpos > 0 ) {
	    title = onames.slice(tabpos+1);
	    onames = onames.slice(0, tabpos);
	}
	e = add_el(td, "span", onames, "oname");
	if ( title ) e.title = title;
    }
    //		Last column is personal rating. It spans two rows.
    //		Top row is ticks/crosses/etc, bottom is short text.
    td = add_el(tr, "td", "", "reaction");
    if ( ri[_rTitle] )
	td.title = ri[_rTitle];
    td.rowSpan = 2;
    add_el(td, "span", ri[_rText], ri[_rClasses]);
    add_el(td, "div", mycomment, ri[_rClasses]);

    //==== Add the second row for this show.
    var tr = _anime_list_table.insertRow(-1), td, e;
    tr.className="row2";
    //		First cell is either tags OR
    //			“<details><summary>TAGS</summary><div>DESC</div></details>”.
    if ( desc ) {
	td = add_el(tr, "td", "", "tags-desc");
	e = add_el(td, "details");
	add_el(e, "summary", tags.join(", "), "tags");
	e = add_el(e, "div")
	e.innerHTML = desc;
    } else {
	add_el(tr, "td", tags.join(", "), "tags");
    }
    //		Second cell is site and (if show is weekly) dow/tod.
    if ( site == UnknownSite ) {
	td = add_el(tr, "td", si[_siteAbbrev], "waw unknown-site");
	td.title = si[_siteName];
    } else {
	if ( sitepath == "X" ) {
	    td = add_el(tr, "td", "", "waw");
	    e = td;
	} else {
	    if ( sitepath.substring(0,4) != "http" )
		sitepath = si[_siteBase]+sitepath;
	    td = add_el(tr, "td", "", "waw", sitepath);
	    e = td.firstChild;	// The <a href=...> element
	}
	if ( schedule ) {
	    add_el(e, "span", si[_siteAbbrev], "site");
	    if ( schedule[0].startsWith("2") ) {
		add_el(e, "span", schedule[0], "year")
		add_el(e, "span", "-"+schedule[1], "month")
	    } else {
		add_el(e, "span", schedule[0], "dow");
		add_el(e, "span", schedule[1], (schedule[1] != "?") ? "hhmm" : "no-hhmm")
	    }
	    e.title = si[_siteName];
	} else {
	    td.classList.add("no-schedule");
	    e.appendChild(document.createTextNode(si[_siteName]));
	}
    }
    //		Third cell is TVTropes.org link, if any.
    if ( tvt !== NoTvT) {
	add_el(tr, "td", "TvT", "TVTlink", TVT_base+tvt);
    } else {
	add_el(tr, "td", EMDASH, "TVTlink");
    }
    //		Fourth cell is Wikipedia link, if any.
    if ( wp !== NoWP ) {
	add_el(tr, "td", "WP", "WPlink", WP_base+wp);
    } else {
	add_el(tr, "td", EMDASH, "WPlink");
    }
 } catch (e) { alert(e); }
}
