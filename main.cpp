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
 * Last modified: Thu Oct 31 22:03:36 KST 2013
 */

#include <iostream>
#include <vector>
#include <string>
#include <iterator>
#include <memory>

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

// std::ostream& operator<<( std::ostream &os, CXCursor cursor )
// {
//   auto spelling = clang_getCursorKindSpelling( cursor.kind );
//   os << clang_getCString( spelling );
//   clang_disposeString( spelling );
//   return os;
// }

std::ostream& operator<<( std::ostream &os, CXCursor cursor )
{
	auto spelling = clang_getCursorKindSpelling( cursor.kind );
	auto morespell = clang_getCursorSpelling( cursor );

	os << clang_getCString( spelling ) << ":"
	   << clang_getCString( morespell ) << " ";

	clang_disposeString( spelling );
	clang_disposeString( morespell );

	auto refcursor = clang_getCursorReferenced( cursor );
	if ( clang_equalCursors( refcursor, clang_getNullCursor() )||
	     clang_equalCursors( refcursor, cursor ) )
		return os;

	return os << " [ " << refcursor << " ] ";
}

std::ostream& operator<<( std::ostream& os, CXSourceLocation loc )
{
	CXFile cxfile;
	unsigned line, col, off;
	clang_getFileLocation( loc, &cxfile, &line, &col, &off );
	auto filestr = clang_getFileName( cxfile );
	os << clang_getCString( filestr ) << ":" << line << ":" << col;
	clang_disposeString( filestr );
	return os;
}

std::ostream& operator<<( std::ostream& os, CXSourceRange range )
{
	auto beg = clang_getRangeStart( range );
	auto end = clang_getRangeEnd( range );
	return os << beg << " -> " << end << "\n";
}

std::ostream& operator<<( std::ostream& os, CXType type )
{
	auto typestr = clang_getTypeSpelling( type );
	os << "Type: " << clang_getCString( typestr ) << "\n";
	clang_disposeString( typestr );
	return os;
}

std::ostream& operator<<( std::ostream& os, CXString str )
{
	os << clang_getCString( str );
	return os;
}

CXChildVisitResult
CallExprVisitor( CXCursor cursor, CXCursor parent, CXClientData d )
{
	std::string *result = reinterpret_cast<std::string *>( d );

	std::cout << "Child of CallExpr = " << cursor << "\n";

	if ( cursor.kind == CXCursor_TypeRef )
		*result = "Identifier";
	//    *result = "Variable";

	return CXChildVisit_Break;
}

std::string AdvancedCallExprHandler( CXCursor cursor )
{
	auto newcursor = clang_getCursorReferenced( cursor );
	std::string result = "Function";

	if ( newcursor.kind == CXCursor_Constructor )
		result = "Identifier";
	else
		clang_visitChildren( cursor, CallExprVisitor,
		                     reinterpret_cast<std::string *>(&result) );
	return result;
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

	case CXCursor_CallExpr:
		return AdvancedCallExprHandler( cursor );

	case CXCursor_CXXMethod:
	case CXCursor_FunctionDecl:
	case CXCursor_Constructor:
	case CXCursor_Destructor:
	case CXCursor_OverloadedDeclRef:
		return "Function";

	case CXCursor_ParmDecl:
	case CXCursor_VarDecl:
	case CXCursor_FieldDecl:
	case CXCursor_MemberRef:
	case CXCursor_VariableRef:
	case CXCursor_NonTypeTemplateParameter:
		return "Variable";

	case CXCursor_NamespaceRef:
		return "Namespace";

	default:
		return "Identifier";
	}

	return {};
}

std::vector<CXCursor>
my_annotateTokens( CXTranslationUnit tu, CXToken *tokens,
                   unsigned token_count )
{
	std::vector<CXCursor> cursors( token_count );
	for ( auto n = 0u; n < token_count; ++n ) {
		auto location = clang_getTokenLocation( tu, tokens[ n ] );
		cursors[n] = ( clang_getCursor( tu, location ) );
	}

	return cursors;
}

void TokenizeSource(CXTranslationUnit tu)
{
	CXSourceRange range =
		clang_getCursorExtent( clang_getTranslationUnitCursor(tu) );

	CXToken *tokens;
	unsigned int token_count;

	clang_tokenize( tu, range, &tokens, &token_count );

	//CXCursor cursors[ token_count ];
	//clang_annotateTokens( tu, tokens, token_count, cursors );
	auto cursors = my_annotateTokens( tu, tokens, token_count );

	for ( auto t = 0u; t < token_count; ++t ) {
		auto tkind = clang_getTokenKind(tokens[t] );
		auto tspelling = tkind == CXToken_Identifier ?
			CursorKindSpelling( cursors[ t ] ) :
			TokenKindSpelling( tkind );
		auto textent = clang_getTokenExtent( tu, tokens[t] );
		auto tstartloc = clang_getRangeStart( textent );
		auto tendloc = clang_getRangeEnd( textent );

		auto tokspell = clang_getTokenSpelling( tu, tokens[ t ] );
		std::cout << "TokenSpelling: " << tokspell << "\n";
		std::cout << "Cursor: " << cursors[ t ] << "\n";

		// if ( !( cursors[t].kind >= CXCursor_FirstInvalid &&
		// 	    cursors[t].kind <= CXCursor_LastInvalid ) ) {
		//   auto rr = clang_getCursorExtent( cursors[ t ] );
		//   std::cout << "Range: " << rr << "\n";
		// }

		// std::cout << clang_getCursorDisplayName( cursors[ t ] ) << "\n";
		// std::cout << "USR: " << clang_getCursorUSR( cursors[ t ] ) << "\n";

		unsigned int startoffset, endoffset;
		clang_getSpellingLocation( tstartloc, nullptr, nullptr, nullptr, &startoffset );
		clang_getSpellingLocation( tendloc, nullptr, nullptr, nullptr, &endoffset );

		// TODO: testing this hack for int -> identifier instead of keyword
		// but this loses const to an identifier also! fvck!
		if ( tspelling == "Keyword" ) {
			auto type = clang_getCursorType( cursors[ t ] );
			auto typekind = type.kind;
			CXString typespelling;

			if ( cursors[t].kind == CXCursor_FunctionDecl ||
			     cursors[t].kind == CXCursor_CXXMethod ) {
				type = clang_getResultType( type );
				typekind = type.kind;
				typespelling = clang_getTypeSpelling( type );
			}
			else
				typespelling = clang_getTypeSpelling( type );

			// std::cout << "Type = " << type << " kind: " << typekind << "\n";
			// std::cout << clang_getCString(typespelling) << " <-> " << clang_getCString(tokspell) << "\n";
			// std::cout << " Const? " << clang_isConstQualifiedType( type ) << "\n";

			if ( (( typekind >= CXType_FirstBuiltin && typekind <= CXType_LastBuiltin ) &&
			      ( std::string(clang_getCString(typespelling)) ==
			        std::string(clang_getCString(tokspell) ) )) ||
			     //	   ( cursors[t].kind == CXCursor_VarDecl ) ||
			     ( cursors[t].kind == CXCursor_ParmDecl ) )
				tspelling = "Identifier";
		}

		//if ( tspelling != "Punctuation" )
		std::cout
			<< startoffset << ":" << endoffset << " @ "
			<< tspelling  << "\n";

		clang_disposeString( tokspell );
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

struct ArgList
{
	std::vector<std::string> args;
	std::unique_ptr<const char *> c_args;

	ArgList(int argc, char **argv)
		: args( argv, argv+argc ), c_args( new const char *[argc] )
	{ build_c_args(); }

	ArgList( std::vector<std::string> vargs )
		: args( vargs ), c_args( new const char *[ args.size() ] )
	{ build_c_args(); }

	void build_c_args()
	{
		auto i = 0u;
		for ( auto s : args )
			std::cout << s << " ";
		std::cout << std::endl;

		for ( auto &s : args )
			(c_args.get())[ i++ ] = s.c_str();
	}

	operator const char **()
	{
		return c_args.get();
	}

	std::size_t count() const
	{ return args.size(); }
};

struct TUnit
{
	CXIndex index;
	CXTranslationUnit unit;
	std::string filename;
	bool valid;
	unsigned const options = clang_defaultEditingTranslationUnitOptions();
	int orig_argc;
	const char **orig_argv;

	TUnit( CXIndex idx, std::string file )
		: index( idx ), filename(  file )
	{ std::cout << "File: " << filename << "\n"; }

	~TUnit()
	{ clang_disposeTranslationUnit( unit ); }

	bool parse( int argc, const char **argv,
	            CXUnsavedFile *unsaved_files = nullptr,
	            unsigned int unsaved_file_count = 0 )
	{
		orig_argc = argc;
		orig_argv = argv;
		for ( auto i = 0; i < argc; ++i )
			std::cout << argv[i] << " ";
		std::cout << "\n";

		unit = clang_parseTranslationUnit( index,
		                                   filename.c_str(),
		                                   argv,
		                                   argc,
		                                   unsaved_files,
		                                   unsaved_file_count,
		                                   options );
		return valid = (unit != NULL);
	}

	CXTranslationUnit handle()
	{ return unit; }

	bool parse( std::vector<CXUnsavedFile> unsavedfiles )
	{
		if ( !valid ) {
			std::cout << "Warning, not valid!\n";
			if ( !parse( orig_argc, orig_argv,
			             unsavedfiles.data(),
			             unsavedfiles.size() ) )
				return false;
		}

		valid = !clang_reparseTranslationUnit( unit,
		                                       unsavedfiles.size(),
		                                       unsavedfiles.data(),
		                                       options );

		return valid;
	}
};

int main(int argc, char *argv[])
{
	auto index = clang_createIndex(0, 0);
	std::vector<std::string> default_args = { {"-x"}, {"c++"}, {"-std=c++11"} };
	std::string filename;
	ArgList arglist( default_args );

	if ( argc > 1 ) {
		arglist = ArgList( argc - 2, argv + 1 );
		filename = argv[argc - 1];
	} else {
		filename = argv[1];
	}

	TUnit tu( index, filename );

	if ( !tu.parse( arglist.count(), arglist ) ) {
		std::cout << "Translation Unit Initial Parse Failed!\n";
	}

	std::string input;
	std::vector<char> filebuffer;
	while( std::getline( std::cin, input ) ) {
		if ( input == "REPARSE" ) {
			filebuffer = ReparseSource();

			CXUnsavedFile unsaved_file = { filename.c_str(),
			                               filebuffer.data(),
			                               filebuffer.size() };

			// std::cout << "Size = " << filebuffer.size()
			// 		<< "Contents:\n" << filebuffer.data()
			// 		<< "\n";

			if ( tu.parse( std::vector<CXUnsavedFile>( 1, unsaved_file ) ) ) {
				TokenizeSource( tu.handle() );
			} else {
				std::cout << "Reparse FAILED!\n" << end_pattern << "\n";
			}
		}
	}

	clang_disposeIndex( index );
	return 0;
}
