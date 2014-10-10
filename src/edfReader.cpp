#include <Rcpp.h>
using namespace Rcpp;

#include <edf.h>

// [[Rcpp::export]]
SEXP edfReader(std::string fileName)
{

  int err;
  EDFFILE *file = edf_open_file(fileName.c_str(), 1, 1, 1, &err);

  if(!file || err) return R_NilValue;

  while((int type = edf_get_next_data(file)) != NO_PENDING_ITEMS)
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
      fd->fs;
      break;
    }
  }

  edf_close_file(file);


  CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
  NumericVector y   = NumericVector::create( 0.0, file?1:0 ) ;
  List z            = List::create( x, y ) ;

  return z ;
}
