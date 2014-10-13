#include <Rcpp.h>
using namespace Rcpp;

#include <edf.h>

// [[Rcpp::export]]
List edfReader(std::string fileName)
{

  int err;
  EDFFILE *file = edf_open_file(fileName.c_str(), 1, 1, 1, &err);

  std::vector<int>
    sTime, sFlags,
    sInput, sButtons,
    htype, hdata0, hdata1, hdata2, hdata3, hdata4, hdata5, hdata6, hdata7,
    sErrors
    ;
  
  std::vector<double>
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
  std::vector<double>
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
    
  // @todo: preallocate all that shit


  if(!file || err) return R_NilValue;
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
      DO_COPY(eType, type);
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
        eMessage.push_back(std::string(fd->fe.message));
    
#undef DO_COPY
      break;
      
    case RECORDING_INFO:
      fd->rec;
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
   
#undef CP
#undef COD
  edf_close_file(file);


  int begin = 0;
  int end = 0;
  
#define C(name) _[#name] = name
  
  DataFrame samples = DataFrame::create(
      _["flags"] = sFlags,
      _["input"] = sInput,
      _["buttons"] = sButtons,
//htype, hdata0, hdata1, hdata2, hdata3, hdata4, hdata5, hdata6, hdata7,
      _["errors"] = sErrors,
      C(pxL), 		C(pxR),
      C(pyL), 		C(pyR),
      C(hxL), 		C(hxR), C(hyL), C(hyR),
      C(paL), 		C(paR),
      C(gxL), 		C(gxR),
      C(gyL), 		C(gyR),
      C(rx), 		C(ry),
      C(gxvelL, 	C(gxvelR),
      C(gyvelL, 	C(gyvelR),
      C(hxvelL, 	C(hxvelR),
      C(hyvelL, 	C(hyvelR),
      C(rxvelL, 	C(rxvelR),
      C(ryvelL, 	C(ryvelR),
      C(fgxvelL, 	C(fgxvelR),
      C(fgyvelL, 	C(fgyvelR),
      C(fhxvelL, 	C(fhxvelR),
      C(fhyvelL, 	C(fhyvelR),
      C(frxvelL, 	C(frxvelR),
      C(fryvelL,	C(fryvelR)
    );
  DataFrame events = DataFrame::create(
	  _["type"] = eType,
	  _["stTime"] = eStTime,
	  _["enTime"] = eEnTime,
      _["status"] = eStatus,
 	  _["flags"] = eFlags,
 	  _["input"] = eInput, 
	  _["buttons"] = eButtons,
      _["parsedBy"] = eParsedBy,
      _["message"] = eMessage,

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
  );

  return List::create(
      _["samples"] = samples,
      _["events"] = events,
      _["begin"] = begin,
      _["end"] = end
    ) ;
}
