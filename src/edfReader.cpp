#include <Rcpp.h>
using namespace Rcpp;

#include <edf.h>

// [[Rcpp::export]]
SEXP edfReader(std::string fileName)
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
    rx, ry,
    gxvelL, gxvelR,
    gyvelL, gyvelR,
    hxvelL, hxvelR,
    hyvelL, hyvelR,
    rxvelL, ryvelR,
    fgxvelL, fgxvelR,
    fgyvelL, fgyvelR,
    fhxvelL, fhxvelR,
    fhyvelL, fhyvelR,
    frxvelL, frxvelR,
    fryvelL,fryvelR
    ;
    
  // @todo: preallocate all that shit


  if(!file || err) return R_NilValue;
  int type;
  while((type = edf_get_next_data(file)) != NO_PENDING_ITEMS)
  {
    ALLF_DATA *fd = edf_get_float_data(file);
    
    switch(type)
    {
    case STARTSACC:
		case STARTFIX:
		case STARTPARSE:

		case ENDBLINK:
		case ENDSACC:
		case ENDFIX:
		case ENDPARSE:

		case FIXUPDATE:
		case BREAKPARSE:
		case BUTTONEVENT:
		case INPUTEVENT:
		case MESSAGEEVENT: 
	
    case STARTSAMPLES:
		case STARTEVENTS:
		case ENDSAMPLES:
		case ENDEVENTS:
      fd->fe;
      break;
      
    case RECORDING_INFO:
      fd->rec;
      break;
    
    case SAMPLE_TYPE: 
    
#define DO_COPY(TO, FROM) TO.push_back(fd->fs.FROM)
#define COD(name)  DO_COPY(name#L, name[0]); DO_COPY(name#R, name[1])
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

  edf_close_file(file);


  CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
  NumericVector y   = NumericVector::create( 0.0, file?1:0 ) ;
  List z            = List::create( x, y ) ;

  return z ;
}
