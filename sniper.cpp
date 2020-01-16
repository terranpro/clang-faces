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
 * sniper.cpp
 *
 * Author: Brian Fransioli
 * Created: Sat Aug 16:35:53 KST 2013
 * Last modified: Tue Jan 15 17:17:12 KST 2019
 */

#include <iostream>
#include <vector>
#include <string>
#include <iterator>
#include <sstream>
#include <iomanip>

#include "clang-c/Index.h"

std::string filename;
CXTranslationUnit tu;

std::ostream& operator<<( std::ostream &os, CXCursor cursor )
{
  auto spelling = clang_getCursorKindSpelling( cursor.kind );
  auto morespell = clang_getCursorSpelling( cursor );

  os << clang_getCString( spelling ) << ":" << clang_getCString( morespell ) << " ";

  clang_disposeString( spelling );
  clang_disposeString( morespell );

  //if ( clang_isReference( cursor.kind ) ) {
  auto refcursor = clang_getCursorReferenced( cursor );
  if ( clang_equalCursors( refcursor, clang_getNullCursor() )||
       clang_equalCursors( refcursor, cursor ) )
    return os;

  os << " [ " << refcursor << " ] ";
    //  }

  return os;
}

std::ostream& operator<<( std::ostream& os, CXSourceLocation location )
{
  unsigned int line, col, offset;
  clang_getSpellingLocation( location, NULL, &line, &col, &offset );
  return os << line << ":" << col << "(" << offset << ")";
}

std::ostream& operator<<( std::ostream& os, CXSourceRange range )
{
  auto start = clang_getRangeStart( range );
  auto end = clang_getRangeEnd( range );
  return os << start << " -> " << end;
}

CXChildVisitResult visitor( CXCursor cursor, CXCursor parent, CXClientData d )
{
  auto range = clang_getCursorExtent( cursor );
  auto space = reinterpret_cast<long>( d );
  if ( space > 0 )
    std::cout << std::setw( space ) << " ";
  std::cout << cursor << " " << range << "\n";
  space += 2;

  auto location = clang_getCursorLocation( cursor );
  CXFile thisfile;
  clang_getSpellingLocation( location, &thisfile, NULL, NULL, NULL );

  if ( cursor.kind != CXCursor_InclusionDirective &&
       clang_getFile( tu, filename.c_str() ) == thisfile )
    clang_visitChildren( cursor, visitor, reinterpret_cast<CXClientData>(space) );

  return CXChildVisit_Continue;
}

int main(int argc, char *argv[])
{
  auto index = clang_createIndex(0, 0);
  auto options = clang_defaultEditingTranslationUnitOptions();
  char const *args[] = { "-x", "c++", "-std=c++11" };
  auto arg_count = sizeof( args ) / sizeof( *args );
  filename = argv[1];

  CXUnsavedFile *unsaved_files = NULL;
  auto unsaved_file_count = 0;

  tu = clang_parseTranslationUnit(index, filename.c_str(), args, arg_count,
				  unsaved_files, unsaved_file_count,
				  options );

  if ( !tu ) {
    std::cout << "Translation Unit Parse Failed!\n";
    return -1;
  }

  std::stringstream ss( argv[2] );
  int line, col;

  ss >> line;
  ss.get();
  ss >> col;
  std::cout << "Hello " << line << ":" << col << "\n";

  auto file = clang_getFile( tu, filename.c_str() );
  auto location = clang_getLocation( tu, file, line, col );


  clang_visitChildren( clang_getTranslationUnitCursor( tu ), visitor,
		       reinterpret_cast<CXClientData>(0) );

  auto cursor = clang_getCursor( tu, location );
  auto refcursor = clang_getCursorReferenced( cursor );
  auto rrefcursor = clang_getCursorReferenced( refcursor );
  auto arf = clang_getTypeKindSpelling( clang_getCursorType( cursor ).kind );
  auto foo = clang_getCanonicalCursor( cursor );
  auto semparent = clang_getCursorSemanticParent( cursor );
  auto lexparent = clang_getCursorLexicalParent( cursor );

  std::cout << cursor << "\n";
  std::cout << refcursor << "\n";
  std::cout << rrefcursor << "\n";
  std::cout << clang_getCString(arf) << "\n";
  std::cout << foo << "\n";
  std::cout << "Parent: " << semparent << "\n";
  std::cout << "LexParent: " << lexparent << "\n";

  //clang_visitChildren( semparent, visitor, reinterpret_cast<CXClientData>(0) );
  clang_disposeString( arf );

  return 0;
}
