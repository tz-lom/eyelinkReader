#include <Rcpp.h>
using namespace Rcpp;

#include <edf.h>
#include <opt.h>

class ListBuilder {

public:

  ListBuilder() {};
  ~ListBuilder() {};

  inline ListBuilder& add(const std::string& name, SEXP x) {
    names.push_back(name);
    elements.push_back(PROTECT(x));
    return *this;
  }

  template <typename T>
  inline ListBuilder& add(const std::string& name, const T& x) {
    names.push_back(name);
    elements.push_back(PROTECT(wrap(x)));
    return *this;
  }

  inline operator List() const {
    List result(elements.size());
    for (size_t i = 0; i < elements.size(); ++i) {
      result[i] = elements[i];
    }
    result.attr("names") = wrap(names);
    UNPROTECT(elements.size());
    return result;
  }

  inline operator DataFrame() const {
    List result = static_cast<List>(*this);
    result.attr("class") = "data.frame";
    result.attr("row.names") = IntegerVector::create(NA_INTEGER, XLENGTH(elements[0]));
    return result;
  }

private:

  std::vector<std::string> names;
  std::vector<SEXP> elements;

  ListBuilder(ListBuilder const&) {};

};

// [[Rcpp::export("read.edf")]]
List edfReader(std::string fileName)
{
  int err;
  EDFFILE *file = edf_open_file(fileName.c_str(), 1, 1, 1, &err);
  if(!file || err) return R_NilValue;


  std::vector<int>
    sTime, sFlags,
    sInput, sButtons,
    htype, hdata0, hdata1, hdata2, hdata3, hdata4, hdata5, hdata6, hdata7,
    sErrors
    ;
//  
  std::vector<float>
    pxL, pxR, pyL, pyR,
    hxL, hxR, hyL, hyR,
    paL, paR,
    gxL, gxR,
    gyL, gyR,
    rx, ry,
    gxvelL, gxvelR,
    gyvelL, gyvelR,
    hxvelL, hxvelR,
    hyvelL, hyvelR,
    rxvelL, rxvelR,
    ryvelL, ryvelR,
    fgxvelL, fgxvelR,
    fgyvelL, fgyvelR,
    fhxvelL, fhxvelR,
    fhyvelL, fhyvelR,
    frxvelL, frxvelR,
    fryvelL,fryvelR
    ;
    
  std::vector<int>
    eTime, eType,
    eStTime, eEnTime,
    eStatus, eFlags, eInput, eButtons,
    eParsedBy
    ;
  std::vector<std::string>
    eMessage
    ;
  std::vector<float>
    hstx, hsty,
    gstx, gsty,
    sta,
    henx, heny,
    genx, geny,
    ena,
    havx, havy,
    gavx, gavy,
    ava,
    avel,
    pvel,
    svel, evel,
    supd_x, eupd_x,
    supd_y, eupd_y
    ;
  ListBuilder _begin, _end;
    
  // @todo: preallocate all that shit
#define P(name) name.reserve(p_size)

  int p_size = edf_get_element_count(file);
  
  P(sTime);
  P(sFlags);
  P(sInput);
  P(sButtons);
    
  P(htype);
  P(hdata0);
  P(hdata1);
  P(hdata2);
  P(hdata3);
  P(hdata4);
  P(hdata5);
  P(hdata6);
  P(hdata7);
  P(sErrors);
  
  P(pxL); P(pxR); P(pyL); P(pyR);
  P(hxL); P(hxR); P(hyL); P(hyR);
  P(paL); P(paR);
  P(gxL); P(gxR);
  P(gyL); P(gyR);
  P(rx); P(ry);
  P(gxvelL); P(gxvelR);
  P(gyvelL); P(gyvelR);
  P(hxvelL); P(hxvelR);
  P(hyvelL); P(hyvelR);
  P(rxvelL); P(rxvelR);
  P(ryvelL); P(ryvelR);
  P(fgxvelL); P(fgxvelR);
  P(fgyvelL); P(fgyvelR);
  P(fhxvelL); P(fhxvelR);
  P(fhyvelL); P(fhyvelR);
  P(frxvelL); P(frxvelR);
  P(fryvelL); P(fryvelR);
  
   P(eTime); P( eType); 
    P(eStTime); P( eEnTime); 
    P(eStatus); P( eFlags); P( eInput); P( eButtons); 
    P(eParsedBy);
    P(eMessage);
    P(hstx); P( hsty); 
    P(gstx); P( gsty); 
    P(sta); 
    P(henx); P( heny); 
    P(genx); P( geny); 
    P(ena); 
    P(havx); P( havy); 
    P(gavx); P( gavy); 
    P(ava); 
    P(avel); 
    P(pvel); 
    P(svel); P( evel); 
    P(supd_x); P( eupd_x); 
    P(supd_y); P( eupd_y);
  
#undef P

  int type;
  while((type = edf_get_next_data(file)) != NO_PENDING_ITEMS)
  {
    ALLF_DATA *fd = edf_get_float_data(file);

#define COD(name)  DO_COPY(name##L, name[0]); DO_COPY(name##R, name[1])
#define CP(name) DO_COPY(name, name)

    switch(type)
    {
    case STARTBLINK:
    case STARTSACC:
		case STARTFIX:
    case STARTSAMPLES:
  	case STARTEVENTS:
		case STARTPARSE:

		case ENDBLINK:
		case ENDSACC:
		case ENDFIX:
    case ENDSAMPLES:
  	case ENDEVENTS:
		case ENDPARSE:

		case FIXUPDATE:
		case BREAKPARSE:
		case BUTTONEVENT:
		case INPUTEVENT:
		case MESSAGEEVENT: 
#define DO_COPY(TO, FROM) TO.push_back(fd->fe.FROM)    

      DO_COPY(eTime, time);
      {
        int cType[] = {
        /*    /  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F */
        /* 1 */  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11, 1, 1, 1, 1,12,
        /* 1 */ 13,14,15, 1, 1, 1, 1, 1,16,17, 1, 1,18, 1, 1, 1,
        /* 2 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* 3 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,19,
        /* 4 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* 5 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* 6 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* 7 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* 8 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* 9 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* A */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* B */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* C */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* D */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* E */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        /* F */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
        };
        eType.push_back(cType[type]);
      }
      DO_COPY(eStTime, sttime);
      DO_COPY(eEnTime, entime);
      DO_COPY(eStatus, status);
      DO_COPY(eFlags, flags);
      DO_COPY(eInput, input);
      DO_COPY(eButtons, buttons);
      DO_COPY(eParsedBy, parsedby);
      
      CP(hstx);
      CP(hsty);
      CP(gstx);
      CP(gsty);
      CP(sta);
      CP(henx);
      CP(heny);
      CP(genx);
      CP(geny);
      CP(ena);
      CP(havx);
      CP(havy);
      CP(gavx);
      CP(gavy);
      CP(ava);
      CP(avel);
      CP(pvel);
      CP(svel);
      CP(evel);
      CP(supd_x);
      CP(eupd_x);
      CP(supd_y);
      CP(eupd_y);
    
      if(fd->fe.message)
        eMessage.push_back(std::string(&fd->fe.message->c, fd->fe.message->len));
      else
        eMessage.push_back(std::string());
    
#undef DO_COPY
      break;
      
    case RECORDING_INFO:
      {
        ListBuilder *obj;
        if(fd->rec.state==1)
        {
          //start
          obj = &_begin;
        }
        else
        {
          //end
          obj = &_end;
        }
        (*obj).add("time", fd->rec.time);
        (*obj).add("record", fd->rec.record_type);
        (*obj).add("pupil", fd->rec.pupil_type);
        (*obj).add("mode", fd->rec.recording_mode);
        (*obj).add("filter", fd->rec.filter_type);
        (*obj).add("sampleRate", fd->rec.sample_rate);
        (*obj).add("type", fd->rec.pos_type);
        (*obj).add("eyes", fd->rec.eye);
      }
      break;
    
    case SAMPLE_TYPE: 
    
#define DO_COPY(TO, FROM) TO.push_back(fd->fs.FROM)
      DO_COPY(sTime, time);
      DO_COPY(sFlags, flags);
      
      DO_COPY(sInput, input);
      DO_COPY(sButtons, buttons);
      DO_COPY(htype, htype);
      DO_COPY(hdata0, hdata[0]);
      DO_COPY(hdata1, hdata[1]);
      DO_COPY(hdata2, hdata[2]);
      DO_COPY(hdata3, hdata[3]);
      DO_COPY(hdata4, hdata[4]);
      DO_COPY(hdata5, hdata[5]);
      DO_COPY(hdata6, hdata[6]);
      DO_COPY(hdata7, hdata[7]);
      DO_COPY(sErrors, errors);
      
      
      COD(px);
      COD(py);
      COD(hx);
      COD(hy);
      COD(pa);
      COD(gx);
      COD(gy);
      
      DO_COPY(rx, rx);
      DO_COPY(ry, ry);
      
      COD(gxvel);
      COD(gyvel);
      COD(hxvel);
      COD(hyvel);
      COD(rxvel);
      COD(ryvel);
      COD(fgxvel);
      COD(fgyvel);
      COD(fhxvel);
      COD(fhyvel);
      COD(frxvel);
      COD(fryvel);

#undef DO_COPY

      break;
    }
  }
  
  List begin = _begin,
       end = _end;
   
#undef CP
#undef COD
  edf_close_file(file);

// dickheads from SR research found funny to use two types of NaN for pointing missed data
#define C(name) { NumericVector vec = wrap(name); _samples.add(#name, ifelse((vec == NaN) + (vec == MISSING_DATA), NA_REAL, vec)) ; }  
  
  ListBuilder _samples;
  _samples.add("time", sTime);
  _samples.add("flags", sFlags);
  _samples.add("input", sInput);
  
  { IntegerVector vec = wrap(htype); _samples.add("htype", ifelse(vec == MISSING_DATA, NA_INTEGER, vec)); }
  
  _samples.add("buttons", sButtons);
  
  //_samples[hdata0, hdata1, hdata2, hdata3, hdata4, hdata5, hdata6, hdata7,
  _samples.add("errors", sErrors);
  
      C(pxL) 		C(pxR)
      C(pyL) 		C(pyR)
      C(hxL) 		C(hxR) C(hyL) C(hyR)
      C(paL) 		C(paR)
      C(gxL) 		C(gxR)
      C(gyL) 		C(gyR)
      C(rx) 		  C(ry)
      C(gxvelL) 	C(gxvelR)
      C(gyvelL) 	C(gyvelR)
      C(hxvelL) 	C(hxvelR)
      C(hyvelL) 	C(hyvelR)
      C(rxvelL) 	C(rxvelR)
      C(ryvelL) 	C(ryvelR)
      C(fgxvelL) 	C(fgxvelR)
      C(fgyvelL) 	C(fgyvelR)
      C(fhxvelL) 	C(fhxvelR)
      C(fhyvelL)	C(fhyvelR)
      C(frxvelL)	C(frxvelR)
      C(fryvelL)	C(fryvelR)
      
  DataFrame samples = _samples;

#undef C
#define C(name) { NumericVector vec = wrap(name); _events.add(#name, ifelse(vec == NaN, NA_REAL, vec)) ; }

  ListBuilder _events;
  
  _events.add("time", eTime);
  
  IntegerVector vType = wrap(eType);
  vType.attr("levels") =  
    CharacterVector::create(
      "",
      "startparse",
      "endparse",
      "startblink",
      "endblink",
      "startsacc",
      "endsacc",
      "startfix",
      "endfix",
      "fixupdate",
      "breakparse",
      "startsamples",
      "endsamples",
      "startevents",
      "endevents",
      "message",
      "button",
      "input",
      "lostData"
      );
  vType.attr("class") = "factor";
  _events.add("type", vType);
  
	_events.add("stTime", eStTime);
    
	    _events.add("enTime", eEnTime);
      _events.add("status", eStatus);
   	  _events.add("flags", eFlags);
   	  _events.add("input", eInput);
  	  _events.add("buttons", eButtons);
      _events.add("parsedBy", eParsedBy);
      _events.add("message", eMessage);

  		C(hstx)    C(hsty)
  		C(gstx)    C(gsty)
  		C(sta)
  		C(henx)    C(heny)
  		C(genx)    C(geny)
  		C(ena)
  		C(havx)    C(havy)
  		C(gavx)    C(gavy)
  		C(ava)
  		C(avel)
  		C(pvel)
  		C(svel)    C(evel)
  		C(supd_x)  C(eupd_x)
  		C(supd_y)  C(eupd_y)
      
    DataFrame events = _events;

  return List::create(
      _["samples"] = samples,
      _["events"] = events,
      _["begin"] = begin,
      _["end"] = end
    ) ;
}
