/*
 * This file is part of clang-faces
 * 
 * clang-faces is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * clang-faces is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with clang-faces.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/*
 * main.cpp
 * 
 * Author: Brian Fransioli <assem@terranpro.org>
 * Created: Tue Jul 18:55:32 KST 2013
 * Last modified: Wed Jul 31 17:16:04 KST 2013
 */

#include <iostream>
#include <vector>
#include <string>
#include <iterator>

#include "clang-c/Index.h"

const auto end_pattern = "!!!!$$$$!!!!";

std::string TokenKindSpelling( CXTokenKind kind )
{
  switch( kind ) {
  case CXToken_Punctuation: return "Punctuation";

  case CXToken_Keyword: return "Keyword";
    
  case CXToken_Identifier: return "Identifier";
    
  case CXToken_Literal: return "Literal";
    
  case CXToken_Comment: return "Comment";
  default:
    break;
  }
  return {};
}

std::ostream& operator<<( std::ostream &os, CXCursor cursor )
{
  auto spelling = clang_getCursorKindSpelling( cursor.kind );
  os << clang_getCString( spelling );
  clang_disposeString( spelling );
  return os;
}

std::string CursorKindSpelling( CXCursor cursor )
{
  auto kind = cursor.kind;
  CXCursor newcursor;
  
  switch( kind ) {
  case CXCursor_DeclRefExpr:
  case CXCursor_MemberRefExpr:
    newcursor = clang_getCursorReferenced( cursor );
    std::cout << "NEW CURSOR: " << newcursor << "\n";
    return CursorKindSpelling( newcursor );

  case CXCursor_Constructor:
  case CXCursor_Destructor:
  case CXCursor_CXXMethod:
  case CXCursor_CallExpr:
  case CXCursor_FunctionDecl:
    return "Function";

  case CXCursor_ParmDecl:
  case CXCursor_VarDecl:
  case CXCursor_FieldDecl:
  case CXCursor_MemberRef:
    return "Variable";

  case CXCursor_NamespaceRef:
    return "Namespace";
    
  default:
    return "Identifier";
  }

  return {};
}

void TokenizeSource(CXTranslationUnit tu)
{
  CXSourceRange range =
    clang_getCursorExtent( clang_getTranslationUnitCursor(tu) );
  
  CXToken *tokens;
  unsigned int token_count;
  
  clang_tokenize( tu, range, &tokens, &token_count );

  std::cout << "Tokenize found " << token_count << " tokens.\n";

  CXCursor cursors[ token_count ];
  clang_annotateTokens( tu, tokens, token_count, cursors );

  for ( auto t = 0u; t < token_count; ++t ) {
    auto tkind = clang_getTokenKind(tokens[t] );
    auto tspelling = tkind == CXToken_Identifier ?
      CursorKindSpelling( cursors[ t ] ) :
      TokenKindSpelling( tkind );
    auto textent = clang_getTokenExtent( tu, tokens[t] );
    auto tstartloc = clang_getRangeStart( textent );
    auto tendloc = clang_getRangeEnd( textent );

    unsigned startline, startcol, startoffset, endline, endcol, endoffset;
    
    clang_getFileLocation( tstartloc, nullptr, &startline, &startcol,
			   &startoffset );
    clang_getFileLocation( tendloc, nullptr, &endline, &endcol,
			   &endoffset );

    auto cursor_spelling =
      clang_getCursorKindSpelling( cursors[t].kind );

    std::cout << "Cursor Spelling: " << clang_getCString(cursor_spelling )
    	      << "\n";
    
    clang_disposeString( cursor_spelling );

    // TODO: testing this hack for int -> identifier instead of keyword
    // but this loses const to an identifier also! fvck!
    // if ( tspelling == "Keyword" )
    //   if ( clang_getCursorType( cursors[ t ] ).kind != CXType_Invalid )
    // 	tspelling = "Identifier";
    
    std::cout
      << startoffset << ":" << endoffset << " @ "
      << tspelling  << "\n";

    //clang_disposeString( tspelling );
  }

  std::cout << "\n" << end_pattern << "\n";
  
  clang_disposeTokens( tu, tokens, token_count );
}

std::vector<char> ReparseSource()
{
  //std::cout << "Entered ReparseSource!\n";
  std::vector<char> buffer;
  std::string input;
  auto buffer_ins = std::back_inserter( buffer );
  
  while( 1 ) {
    if ( !std::getline( std::cin, input ) )
      return buffer;

    if ( input == end_pattern )
      break;

    std::copy( input.begin(), input.end(),
	       buffer_ins );
    *buffer_ins++ = '\n';
  }

  //*buffer_ins++ = '\0';
  std::cout << "Leaving Reparse Source w/buffer size: "
	    << buffer.size();
  return buffer;
}

int x = 1;

int main(int argc, char *argv[])
{
  auto index = clang_createIndex(0, 0);
  auto options = clang_defaultEditingTranslationUnitOptions();
  char const *args[] = { "-x", "c++", "-std=c++11" };
  auto arg_count = sizeof( args ) / sizeof( *args );
  auto filename = argv[1];
  
  CXUnsavedFile *unsaved_files = NULL;
  auto unsaved_file_count = 0;
  
  auto tu = clang_parseTranslationUnit(index, filename, args, arg_count,
				       unsaved_files, unsaved_file_count,
				       options );
  
  if ( !tu ) {
    std::cout << "Translation Unit Parse Failed!\n";
    return -1;
  }
  
  // else
  //   std::cout << "Translation Unit Created!\n"
  // 	      << end_pattern << "\n";

  //TokenizeSource( tu );

  std::string input;
  std::vector<char> filebuffer;
  while( std::getline( std::cin, input ) ) {
    if ( input == "REPARSE" ) {
      filebuffer = ReparseSource();

      CXUnsavedFile unsaved_file = { filename,
				     filebuffer.data(),
				     filebuffer.size() };

      // std::cout << "Size = " << filebuffer.size()
      // 		<< "Contents:\n" << filebuffer.data()
      // 		<< "\n";

      if ( !clang_reparseTranslationUnit( tu, 1, &unsaved_file, options ) )
	TokenizeSource( tu );
      else {
	std::cout << "Reparse FAILED!\n";
	return -1;
      }
    }
  }

  clang_disposeIndex( index );
  return 0;
}
