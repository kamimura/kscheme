%option noyywrap yylineno
%x IDENTIFIER_VERTICAL_LEX
%x NESTED_COMMENT
%x IMAG IMAG_BIN IMAG_OCT IMAG_HEX
%x IMAG_POLAR IMAG_BIN_POLAR IMAG_OCT_POLAR IMAG_HEX_POLAR
%x STRING_ELEMENT

%{
#include "object.h"
#include "parse.tab.h"

extern int interactive_mode;

GSList *sym_list = NULL;
char sign_imag;
mpfr_t real;
mpfr_t imag;
mpfr_t opfr;
mpfr_t argopfr;
mpfr_t sopfr;
mpfr_t copfr;
mpf_t opf;
mpq_t opq;
%}

intraline_whitespace [ \t]
line_ending (\n|\r\n|\r)

one [\x00-\x7f]                                                
two   ([\xc0-\xdd][\x80-\xbf])
three ([\xe0-\xef][\x80-\xbf]{2})
four  ([\xf0-\xf7][\x80-\xbf]{3})
five  ([\xf8-\xfb][\x80-\xbf]{4})
six   ([\xfc-\xfd][\x80-\xbf]{5})
any_character ({one}|{two}|{three}|{four}|{five}|{six})
one_other_than_vertical_line_or_backslash ([\x00-\x7f]-[\|\\])
any_character_other_than_vertical_line_or_backslash ({one_other_than_vertical_line_or_backslash}|{two}|{three}|{four}|{five}|{six})
any_character_other_than_quote_backslash ([\x00-\x7f]{-}[\"\\]|{two}|{three}|{four}|{five}|{six})
                                                         
%%
 /* whitespace */
{intraline_whitespace}* {}
{line_ending} {}

 /* comment */
";".*{line_ending} {}

 /* id */
 /* ⟨initial⟩ ⟨subsequent⟩* */
([!$%&\*\/:<=>\?^_~a-zA-Z]|([\xc3-\xcc\xcf-\xd1\xd3\xda][\x80-\xbf]|\xc2[\xa1-\xaa\xac\xae-\xba\xbc-\xbf]|\xcd[\x80-\xb7\xba-\xbf]|\xce[\x84-\x8a\x8c\x8e-\xa1\xa3-\xbf]|\xd2[\x80-\x87\x8a-\xbf]|\xd4[\x80-\xaf\xb1-\xbf]|\xd5[\x80-\x96\x99-\x9f\xa1-\xbf]|\xd6[\x80-\x87\x89\x8a\x8d-\x8f\x91-\xbf]|\xd7[\x80-\x87\x90-\xaa\xb0-\xb4]|\xd8[\x86-\x9b\x9e-\xbf]|\xd9[\x80-\x9f\xaa-\xbf]|\xdb[\x80-\x9c\x9e-\xaf\xba-\xbf]|\xdc[\x80-\x8d\x90-\xbf]|\xdd[\x80-\x8a\x8d-\xbf]|\xde[\x80-\xb1]|\xdf[\x8a-\xba])|([\xe1-\xe9\xeb-\xef][\x92\x93\x98\x99][\x80-\xbf]|[\xe1-\xef][\x85\x94-\x97][\x80-\xbf]|[\xe1\xe2\xe4-\xef][\x84\x87\x88][\x80-\xbf]|[\xe1\xe3-\xec\xee\xef]\xba[\x80-\xbf]|[\xe1\xe3-\xec\xee][\xb4\xb5\xb8\xb9\xbb][\x80-\xbf]|[\xe1\xe3-\xef][\x90\x91][\x80-\xbf]|[\xe1\xe3\xe5-\xec\xee]\xb6[\x80-\xbf]|[\xe1\xe4-\xef]\x86[\x80-\xbf]|[\xe1\xea]\xa1[\x80-\xb7]|[\xe2-\xe9\xeb-\xef]\x9b[\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee\xef][\x9e\xa0-\xa5\xa8][\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee][\xa9\xab\xac][\x80-\xbf]|[\xe2-\xec\xee\xef][\xaa\xb2][\x80-\xbf]|[\xe2-\xec\xee][\xbc-\xbe][\x80-\xbf]|[\xe2-\xef][\x89\x8a\x8d\x8e\x9a\x9c][\x80-\xbf]|[\xe2\xe4-\xef]\x8b[\x80-\xbf]|[\xe3-\xe8\xea-\xec\xee]\xbf[\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee\xef][\x9f\xa6\xa7][\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee][\xad\xaf][\x80-\xbf]|[\xe3-\xec\xee\xef][\xae\xb0\xb1\xb3][\x80-\xbf]|[\xe3-\xec\xee]\xb7[\x80-\xbf]|[\xe3-\xef][\x83\x8c\x8f\x9d][\x80-\xbf]|[\xe4-\xef][\x80-\x82][\x80-\xbf]|\xe0\xa0[\x80-\xad\xb0-\xbe]|\xe0\xa1[\x80-\x9b\x9e]|\xe0\xa2[\xa0-\xb4]|\xe0\xa3[\xa3-\xbf]|\xe0\xa4[\x80-\x82\x84-\xba\xbc\xbd]|\xe0\xa5[\x81-\x88\x8d\x90-\xa5\xb0-\xbf]|\xe0\xa6[\x80\x81\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb6-\xb9\xbc\xbd]|\xe0\xa7[\x81-\x84\x8d\x8e\x9c\x9d\x9f-\xa3\xb0-\xbb]|\xe0\xa8[\x81\x82\x85-\x8a\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5\xb6\xb8\xb9\xbc]|\xe0\xa9[\x81\x82\x87\x88\x8b-\x8d\x91\x99-\x9c\x9e\xb0-\xb5]|\xe0\xaa[\x81\x82\x85-\x8d\x8f-\x91\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc\xbd]|\xe0\xab[\x81-\x85\x87\x88\x8d\x90\xa0-\xa3\xb0\xb1\xb9]|\xe0\xac[\x81\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc\xbd\xbf]|\xe0\xad[\x81-\x84\x8d\x96\x9c\x9d\x9f-\xa3\xb0-\xb7]|\xe0\xae[\x82\x83\x85-\x8a\x8e-\x90\x92-\x95\x99\x9a\x9c\x9e\x9f\xa3\xa4\xa8-\xaa\xae-\xb9]|\xe0\xaf[\x80\x8d\x90\xb0-\xba]|\xe0\xb0[\x80\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb9\xbd-\xbf]|\xe0\xb1[\x80\x86-\x88\x8a-\x8d\x95\x96\x98-\x9a\xa0-\xa3\xb8-\xbf]|\xe0\xb2[\x81\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb3\xb5-\xb9\xbc\xbd\xbf]|\xe0\xb3[\x86\x8c\x8d\x9e\xa0-\xa3\xb1\xb2]|\xe0\xb4[\x81\x85-\x8c\x8e-\x90\x92-\xba\xbd]|\xe0\xb5[\x81-\x84\x8d\x8e\x9f-\xa3\xb0-\xb5\xb9-\xbf]|\xe0\xb6[\x85-\x96\x9a-\xb1\xb3-\xbb\xbd]|\xe0\xb7[\x80-\x86\x8a\x92-\x94\x96\xb4]|\xe0\xb8[\x81-\xba\xbf]|\xe0\xb9[\x80-\x8f\x9a\x9b]|\xe0\xba[\x81\x82\x84\x87\x88\x8a\x8d\x94-\x97\x99-\x9f\xa1-\xa3\xa5\xa7\xaa\xab\xad-\xb9\xbb-\xbd]|\xe0\xbb[\x80-\x84\x86\x88-\x8d\x9c-\x9f]|\xe0\xbc[\x80-\x9f\xaa-\xb9]|\xe0\xbd[\x80-\x87\x89-\xac\xb1-\xbe]|\xe0\xbe[\x80-\x97\x99-\xbc\xbe\xbf]|\xe0\xbf[\x80-\x8c\x8e-\x9a]|\xe1\x80[\x80-\xaa\xad-\xb0\xb2-\xb7\xb9\xba\xbd-\xbf]|\xe1\x81[\x8a-\x95\x98-\xa1\xa5\xa6\xae-\xbf]|\xe1\x82[\x80-\x82\x85\x86\x8d\x8e\x9d-\xbf]|\xe1\x83[\x80-\x85\x87\x8d\x90-\xbf]|\xe1\x89[\x80-\x88\x8a-\x8d\x90-\x96\x98\x9a-\x9d\xa0-\xbf]|\xe1\x8a[\x80-\x88\x8a-\x8d\x90-\xb0\xb2-\xb5\xb8-\xbe]|\xe1\x8b[\x80\x82-\x85\x88-\x96\x98-\xbf]|\xe1\x8c[\x80-\x90\x92-\x95\x98-\xbf]|\xe1\x8d[\x80-\x9a\x9d-\xbc]|\xe1\x8e[\x80-\x99\xa0-\xbf]|\xe1\x8f[\x80-\xb5\xb8-\xbd]|\xe1\x9a[\x81-\x9a\xa0-\xbf]|\xe1\x9b[\x80-\xb8]|\xe1\x9c[\x80-\x8c\x8e-\x94\xa0-\xb6]|\xe1\x9d[\x80-\x93\xa0-\xac\xae-\xb0\xb2\xb3]|\xe1\x9e[\x80-\xb5\xb7-\xbd]|\xe1\x9f[\x86\x89-\x9d\xb0-\xb9]|\xe1\xa0[\x80-\x8d\xa0-\xbf]|\xe1\xa2[\x80-\xaa\xb0-\xbf]|\xe1\xa3[\x80-\xb5]|\xe1\xa4[\x80-\x9e\xa0-\xa2\xa7\xa8\xb2\xb9-\xbb]|\xe1\xa5[\x80\x84\x85\x90-\xad\xb0-\xb4]|\xe1\xa6[\x80-\xab\xb0-\xbf]|\xe1\xa7[\x80-\x89\x9a\x9e-\xbf]|\xe1\xa8[\x80-\x98\x9b\x9e-\xbf]|\xe1\xa9[\x80-\x94\x96\x98-\x9e\xa0\xa2\xa5-\xac\xb3-\xbc\xbf]|\xe1\xaa[\xa0-\xad\xb0-\xbd]|\xe1\xac[\x80-\x83\x85-\xb4\xb6-\xba\xbc]|\xe1\xad[\x82\x85-\x8b\x9a-\xbc]|\xe1\xae[\x80\x81\x83-\xa0\xa2-\xa5\xa8\xa9\xab-\xaf\xba-\xbf]|\xe1\xaf[\x80-\xa6\xa8\xa9\xad\xaf-\xb1\xbc-\xbf]|\xe1\xb0[\x80-\xa3\xac-\xb3\xb6\xb7\xbb-\xbf]|\xe1\xb1[\x8d-\x8f\x9a-\xbf]|\xe1\xb3[\x80-\x87\x90-\xa0\xa2-\xb1\xb4-\xb6\xb8\xb9]|\xe1\xb7[\x80-\xb5\xbc-\xbf]|\xe1\xbc[\x80-\x95\x98-\x9d\xa0-\xbf]|\xe1\xbd[\x80-\x85\x88-\x8d\x90-\x97\x99\x9b\x9d\x9f-\xbd]|\xe1\xbe[\x80-\xb4\xb6-\xbf]|\xe1\xbf[\x80-\x84\x86-\x93\x96-\x9b\x9d-\xaf\xb2-\xb4\xb6-\xbe]|\xe2\x80[\x90-\x97\xa0-\xa7\xb0-\xb8\xbb-\xbf]|\xe2\x81[\x80-\x84\x87-\x9e\xb0\xb1\xb4-\xbc\xbf]|\xe2\x82[\x80-\x8c\x90-\x9c\xa0-\xbe]|\xe2\x83[\x90-\x9c\xa1\xa5-\xb0]|\xe2\x86[\x80-\x8b\x90-\xbf]|\xe2\x8c[\x80-\x87\x8c-\xa8\xab-\xbf]|\xe2\x8f[\x80-\xba]|\xe2\x90[\x80-\xa6]|\xe2\x91[\x80-\x8a\xa0-\xbf]|\xe2\x9d[\x80-\xa7\xb6-\xbf]|\xe2\x9f[\x80-\x84\x87-\xa5\xb0-\xbf]|\xe2\xa6[\x80-\x82\x99-\xbf]|\xe2\xa7[\x80-\x97\x9c-\xbb\xbe\xbf]|\xe2\xad[\x80-\xb3\xb6-\xbf]|\xe2\xae[\x80-\x95\x98-\xb9\xbd-\xbf]|\xe2\xaf[\x80-\x88\x8a-\x91\xac-\xaf]|\xe2\xb0[\x80-\xae\xb0-\xbf]|\xe2\xb1[\x80-\x9e\xa0-\xbf]|\xe2\xb3[\x80-\xb3\xb9-\xbf]|\xe2\xb4[\x80-\xa5\xa7\xad\xb0-\xbf]|\xe2\xb5[\x80-\xa7\xaf\xb0\xbf]|\xe2\xb6[\x80-\x96\xa0-\xa6\xa8-\xae\xb0-\xb6\xb8-\xbe]|\xe2\xb7[\x80-\x86\x88-\x8e\x90-\x96\x98-\x9e\xa0-\xbf]|\xe2\xb8[\x80\x81\x86-\x88\x8b\x8e-\x9b\x9e\x9f\xaa-\xbf]|\xe2\xb9[\x80\x81]|\xe2\xba[\x80-\x99\x9b-\xbf]|\xe2\xbb[\x80-\xb3]|\xe2\xbf[\x80-\x95\xb0-\xbb]|\xe3\x80[\x81-\x87\x92\x93\x9c\xa0-\xad\xb0-\xbf]|\xe3\x81[\x81-\xbf]|\xe3\x82[\x80-\x96\x99-\xbf]|\xe3\x84[\x85-\xad\xb1-\xbf]|\xe3\x86[\x80-\x8e\x90-\xba]|\xe3\x87[\x80-\xa3\xb0-\xbf]|\xe3\x88[\x80-\x9e\xa0-\xbf]|\xe3\x8b[\x80-\xbe]|\xe4\xb6[\x80-\xb5]|\xe9\xbf[\x80-\x95]|\xea\x92[\x80-\x8c\x90-\xbf]|\xea\x93[\x80-\x86\x90-\xbf]|\xea\x98[\x80-\x9f\xaa\xab]|\xea\x99[\x80-\xaf\xb3-\xbf]|\xea\x9b[\x80-\xb7]|\xea\x9e[\x80-\xad\xb0-\xb7]|\xea\x9f[\xb7-\xbf]|\xea\xa0[\x80-\xa2\xa5\xa6\xa8-\xab\xb0-\xb9]|\xea\xa2[\x82-\xb3]|\xea\xa3[\x84\x8e\x8f\xa0-\xbd]|\xea\xa4[\x8a-\xbf]|\xea\xa5[\x80-\x91\x9f-\xbc]|\xea\xa6[\x80-\x82\x84-\xb3\xb6-\xb9\xbc]|\xea\xa7[\x81-\x8d\x8f\x9e-\xaf\xba-\xbe]|\xea\xa8[\x80-\xae\xb1\xb2\xb5\xb6]|\xea\xa9[\x80-\x8c\x9c-\xba\xbc\xbe\xbf]|\xea\xab[\x80-\x82\x9b-\xaa\xac\xad\xb0-\xb4\xb6]|\xea\xac[\x81-\x86\x89-\x8e\x91-\x96\xa0-\xa6\xa8-\xae\xb0-\xbf]|\xea\xad[\x80-\xa5\xb0-\xbf]|\xea\xaf[\x80-\xa2\xa5\xa8\xab\xad]|\xed\x9e[\x80-\xa3\xb0-\xbf]|\xed\x9f[\x80-\x86\x8b-\xbb]|\xef\xa9[\x80-\xad\xb0-\xbf]|\xef\xab[\x80-\x99]|\xef\xac[\x80-\x86\x93-\x97\x9d-\xb6\xb8-\xbc\xbe]|\xef\xad[\x80\x81\x83\x84\x86-\xbf]|\xef\xaf[\x80\x81\x93-\xbf]|\xef\xb4[\x80-\xbd]|\xef\xb5[\x90-\xbf]|\xef\xb6[\x80-\x8f\x92-\xbf]|\xef\xb7[\x80-\x87\xb0-\xbd]|\xef\xb8[\x80-\x96\x99\xa0-\xb4]|\xef\xb9[\x85\x86\x89-\x92\x94-\x98\x9f-\xa6\xa8-\xab\xb0-\xb4\xb6-\xbf]|\xef\xbb[\x80-\xbc]|\xef\xbc[\x81-\x87\x8a-\x8f\x9a-\xba\xbc\xbe\xbf]|\xef\xbd[\x80-\x9a\x9c\x9e\xa1\xa4-\xbf]|\xef\xbe[\x80-\xbe]|\xef\xbf[\x82-\x87\x8a-\x8f\x92-\x97\x9a-\x9c\xa0-\xa6\xa8-\xae\xbc\xbd])|([\xf0\xf3]\xa0[\x84-\x86][\x80-\xbf]|\xf0[\x90\x92\x93\x9d\xa0-\xac]\x82[\x80-\xbf]|\xf0[\x90\x92\x93\xa1-\xac]\x85[\x80-\xbf]|\xf0[\x90\x92\x94\x9d\x9f-\xac]\x90[\x80-\xbf]|\xf0[\x90\x94\x9d\x9f-\xac]\x98[\x80-\xbf]|\xf0[\x90\x94\x9f-\xac]\x91[\x80-\xbf]|\xf0[\x90\x9b\xa0-\xac]\xb0[\x80-\xbf]|\xf0[\x90\x9d\x9f-\xac]\x99[\x80-\xbf]|\xf0[\x90\x9d\xa0-\xa9\xab\xac]\x9b[\x80-\xbf]|\xf0[\x90\x9f-\xac]\x9a[\x80-\xbf]|\xf0[\x92\x93\x9d\x9f-\xac][\x81\x8c][\x80-\xbf]|\xf0[\x92\x93\x9d\xa0-\xac][\x80\x88][\x80-\xbf]|\xf0[\x92\x93\x9d\xa1-\xac]\x86[\x80-\xbf]|\xf0[\x92\x93\x9f-\xac]\x8d[\x80-\xbf]|\xf0[\x92\x93\xa0-\xac][\x83\x87\x89-\x8b][\x80-\xbf]|\xf0[\x92\x93\xa1-\xac]\x84[\x80-\xbf]|\xf0[\x92\x94\x9f-\xac][\x92-\x94][\x80-\xbf]|\xf0[\x93\x9f-\xac][\x8e\x8f][\x80-\xbf]|\xf0[\x94\x9d\x9f-\xac]\x97[\x80-\xbf]|\xf0[\x94\x9d\xa0-\xac]\x96[\x80-\xbf]|\xf0[\x94\xa0-\xac]\x95[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xaa\xac\xaf]\xa0[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xac\xaf][\xa1\xa2][\x80-\xbf]|\xf0[\x96\x9d\xa0-\xac\xaf][\xa3-\xa7][\x80-\xbf]|\xf0[\x96\xa0-\xab]\xbc[\x80-\xbf]|\xf0[\x96\xa0-\xac]\xac[\x80-\xbf]|\xf0[\x9d\x9f-\xaa\xac]\x9c[\x80-\xbf]|\xf0[\x9d\x9f-\xac]\x9e[\x80-\xbf]|\xf0[\x9d\xa0-\xac][\x9d\xa8\xa9][\x80-\xbf]|\xf0[\xa0-\xab][\xba\xbb\xbd-\xbf][\x80-\xbf]|\xf0[\xa0-\xac][\x9f\xaa\xab\xad-\xaf\xb1-\xb9][\x80-\xbf]|\xf0\x90\x80[\x80-\x8b\x8d-\xa6\xa8-\xba\xbc\xbd\xbf]|\xf0\x90\x81[\x80-\x8d\x90-\x9d]|\xf0\x90\x83[\x80-\xba]|\xf0\x90\x84[\x80-\x82\x87-\xb3\xb7-\xbf]|\xf0\x90\x86[\x80-\x8c\x90-\x9b\xa0]|\xf0\x90\x87[\x90-\xbd]|\xf0\x90\x8a[\x80-\x9c\xa0-\xbf]|\xf0\x90\x8b[\x80-\x90\xa0-\xbb]|\xf0\x90\x8c[\x80-\xa3\xb0-\xbf]|\xf0\x90\x8d[\x80-\x8a\x90-\xba]|\xf0\x90\x8e[\x80-\x9d\x9f-\xbf]|\xf0\x90\x8f[\x80-\x83\x88-\x95]|\xf0\x90\x92[\x80-\x9d]|\xf0\x90\x94[\x80-\xa7\xb0-\xbf]|\xf0\x90\x95[\x80-\xa3\xaf]|\xf0\x90\x9c[\x80-\xb6]|\xf0\x90\x9d[\x80-\x95\xa0-\xa7]|\xf0\x90\xa0[\x80-\x85\x88\x8a-\xb5\xb7\xb8\xbc\xbf]|\xf0\x90\xa1[\x80-\x95\x97-\xbf]|\xf0\x90\xa2[\x80-\x9e\xa7-\xaf]|\xf0\x90\xa3[\xa0-\xb2\xb4\xb5\xbb-\xbf]|\xf0\x90\xa4[\x80-\x9b\x9f-\xb9\xbf]|\xf0\x90\xa6[\x80-\xb7\xbc-\xbf]|\xf0\x90\xa7[\x80-\x8f\x92-\xbf]|\xf0\x90\xa8[\x80-\x83\x85\x86\x8c-\x93\x95-\x97\x99-\xb3\xb8-\xba\xbf]|\xf0\x90\xa9[\x80-\x87\x90-\x98\xa0-\xbf]|\xf0\x90\xaa[\x80-\x9f]|\xf0\x90\xab[\x80-\xa6\xab-\xb6]|\xf0\x90\xac[\x80-\xb5\xb9-\xbf]|\xf0\x90\xad[\x80-\x95\x98-\xb2\xb8-\xbf]|\xf0\x90\xae[\x80-\x91\x99-\x9c\xa9-\xaf]|\xf0\x90\xb1[\x80-\x88]|\xf0\x90\xb2[\x80-\xb2]|\xf0\x90\xb3[\x80-\xb2\xba-\xbf]|\xf0\x90\xb9[\xa0-\xbe]|\xf0\x91\x80[\x81\x83-\xbf]|\xf0\x91\x81[\x80-\x8d\x92-\xa5\xbf]|\xf0\x91\x82[\x80\x81\x83-\xaf\xb3-\xb6\xb9-\xbc\xbe\xbf]|\xf0\x91\x83[\x80\x81\x90-\xa8]|\xf0\x91\x84[\x80-\xab\xad-\xb4]|\xf0\x91\x85[\x80-\x83\x90-\xb6]|\xf0\x91\x86[\x80\x81\x83-\xb2\xb6-\xbe]|\xf0\x91\x87[\x81-\x8d\x9a-\x9f\xa1-\xb4]|\xf0\x91\x88[\x80-\x91\x93-\xab\xaf-\xb1\xb4\xb6-\xbd]|\xf0\x91\x8a[\x80-\x86\x88\x8a-\x8d\x8f-\x9d\x9f-\xa9\xb0-\xbf]|\xf0\x91\x8b[\x80-\x9f\xa3-\xaa]|\xf0\x91\x8c[\x80\x81\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc\xbd]|\xf0\x91\x8d[\x80\x90\x9d-\xa1\xa6-\xac\xb0-\xb4]|\xf0\x91\x92[\x80-\xaf\xb3-\xb8\xba\xbf]|\xf0\x91\x93[\x80\x82-\x87]|\xf0\x91\x96[\x80-\xae\xb2-\xb5\xbc\xbd\xbf]|\xf0\x91\x97[\x80-\x9d]|\xf0\x91\x98[\x80-\xaf\xb3-\xba\xbd\xbf]|\xf0\x91\x99[\x80-\x84]|\xf0\x91\x9a[\x80-\xab\xad\xb0-\xb5\xb7]|\xf0\x91\x9c[\x80-\x99\x9d-\x9f\xa2-\xa5\xa7-\xab\xba-\xbf]|\xf0\x91\xa2[\xa0-\xbf]|\xf0\x91\xa3[\x80-\x9f\xaa-\xb2\xbf]|\xf0\x91\xab[\x80-\xb8]|\xf0\x92\x8e[\x80-\x99]|\xf0\x92\x91[\x80-\xae\xb0-\xb4]|\xf0\x92\x95[\x80-\x83]|\xf0\x93\x90[\x80-\xae]|\xf0\x94\x99[\x80-\x86]|\xf0\x96\xa8[\x80-\xb8]|\xf0\x96\xa9[\x80-\x9e\xae\xaf]|\xf0\x96\xab[\x90-\xad\xb0-\xb5]|\xf0\x96\xad[\x80-\x85\x9b-\xa1\xa3-\xb7\xbd-\xbf]|\xf0\x96\xae[\x80-\x8f]|\xf0\x96\xbd[\x80-\x84\x90]|\xf0\x96\xbe[\x8f-\x9f]|\xf0\x9b\x80[\x80\x81]|\xf0\x9b\xb1[\x80-\xaa\xb0-\xbc]|\xf0\x9b\xb2[\x80-\x88\x90-\x99\x9c-\x9f]|\xf0\x9d\x83[\x80-\xb5]|\xf0\x9d\x84[\x80-\xa6\xa9-\xbf]|\xf0\x9d\x85[\x80-\xa4\xa7-\xac\xbb-\xbf]|\xf0\x9d\x87[\x80-\xa8]|\xf0\x9d\x89[\x80-\x85]|\xf0\x9d\x8d[\x80-\x96\xa0-\xb1]|\xf0\x9d\x91[\x80-\x94\x96-\xbf]|\xf0\x9d\x92[\x80-\x9c\x9e\x9f\xa2\xa5\xa6\xa9-\xac\xae-\xb9\xbb\xbd-\xbf]|\xf0\x9d\x93[\x80-\x83\x85-\xbf]|\xf0\x9d\x94[\x80-\x85\x87-\x8a\x8d-\x94\x96-\x9c\x9e-\xb9\xbb-\xbe]|\xf0\x9d\x95[\x80-\x84\x86\x8a-\x90\x92-\xbf]|\xf0\x9d\x9a[\x80-\xa5\xa8-\xbf]|\xf0\x9d\x9f[\x80-\x8b]|\xf0\x9d\xaa[\x80-\x8b\x9b-\x9f\xa1-\xaf]|\xf0\x9e\xa3[\x80-\x84\x87-\x96]|\xf0\x9e\xb8[\x80-\x83\x85-\x9f\xa1\xa2\xa4\xa7\xa9-\xb2\xb4-\xb7\xb9\xbb]|\xf0\x9e\xb9[\x82\x87\x89\x8b\x8d-\x8f\x91\x92\x94\x97\x99\x9b\x9d\x9f\xa1\xa2\xa4\xa7-\xaa\xac-\xb2\xb4-\xb7\xb9-\xbc\xbe]|\xf0\x9e\xba[\x80-\x89\x8b-\x9b\xa1-\xa3\xa5-\xa9\xab-\xbb]|\xf0\x9e\xbb[\xb0\xb1]|\xf0\x9f[\x80\x85][\x80-\xab\xb0-\xbf]|\xf0\x9f\x82[\x80-\x93\xa0-\xae\xb1-\xbf]|\xf0\x9f\x83[\x81-\x8f\x91-\xb5]|\xf0\x9f\x84[\x80-\x8c\x90-\xae\xb0-\xbf]|\xf0\x9f\x86[\x80-\x9a]|\xf0\x9f\x87[\xa6-\xbf]|\xf0\x9f\x88[\x80-\x82\x90-\xba]|\xf0\x9f\x89[\x80-\x88\x90\x91]|\xf0\x9f\x95[\x80-\xb9\xbb-\xbf]|\xf0\x9f\x96[\x80-\xa3\xa5-\xbf]|\xf0\x9f\x9b[\x80-\x90\xa0-\xac\xb0-\xb3]|\xf0\x9f\x9d[\x80-\xb3]|\xf0\x9f\x9f[\x80-\x94]|\xf0\x9f\xa0[\x80-\x8b\x90-\xbf]|\xf0\x9f\xa1[\x80-\x87\x90-\x99\xa0-\xbf]|\xf0\x9f\xa2[\x80-\x87\x90-\xad]|\xf0\x9f\xa4[\x90-\x98]|\xf0\x9f\xa6[\x80-\x84]|\xf0\x9f\xa7\x80|\xf0\xaa\x9b[\x80-\x96]|\xf0\xab\x9c[\x80-\xb4]|\xf0\xab\xa0[\x80-\x9d\xa0-\xbf]|\xf0\xac\xba[\x80-\xa1]|\xf0\xaf\xa8[\x80-\x9d]|\xf3[\xb0-\xbe]\xbf[\x80-\xbf]|\xf3[\xb0-\xbf][\x80-\xbe][\x80-\xbf]|\xf3\xa0\x87[\x80-\xaf]|\xf3\xbf\xbf[\x80-\xbd]|\xf4[\x80-\x8e]\xbf[\x80-\xbf]|\xf4[\x80-\x8f][\x80-\xbe][\x80-\xbf]|\xf4\x8f\xbf[\x80-\xbd])|(\xe2\x80\x8c)|(\xe2\x80\x8d))([-\+!$%&\*\/:<=>\?^_~a-zA-Z0-9\.@]|([\xc3-\xcc\xcf-\xd3\xd9\xda][\x80-\xbf]|\xc2[\xa1-\xaa\xac\xae-\xba\xbc-\xbf]|\xcd[\x80-\xb7\xba-\xbf]|\xce[\x84-\x8a\x8c\x8e-\xa1\xa3-\xbf]|\xd4[\x80-\xaf\xb1-\xbf]|\xd5[\x80-\x96\x99-\x9f\xa1-\xbf]|\xd6[\x80-\x87\x89\x8a\x8d-\x8f\x91-\xbf]|\xd7[\x80-\x87\x90-\xaa\xb0-\xb4]|\xd8[\x86-\x9b\x9e-\xbf]|\xdb[\x80-\x9c\x9e-\xbf]|\xdc[\x80-\x8d\x90-\xbf]|\xdd[\x80-\x8a\x8d-\xbf]|\xde[\x80-\xb1]|\xdf[\x80-\xba])|([\xe0\xe2-\xe9\xeb\xec\xee\xef]\xa5[\x80-\xbf]|[\xe0\xe2-\xec\xee\xef]\xa4[\x80-\xbf]|[\xe1-\xe9\xeb-\xef][\x92\x93\x98][\x80-\xbf]|[\xe1-\xe9\xeb\xec\xee\xef]\x9e[\x80-\xbf]|[\xe1-\xe9\xeb\xec\xee]\xac[\x80-\xbf]|[\xe1-\xef][\x85\x94-\x97\x99][\x80-\xbf]|[\xe1\xe2\xe4-\xef][\x84\x87\x88][\x80-\xbf]|[\xe1\xe3-\xec\xee\xef][\xae\xba][\x80-\xbf]|[\xe1\xe3-\xec\xee][\xb4\xb5\xb8\xb9\xbb][\x80-\xbf]|[\xe1\xe3-\xef][\x90\x91][\x80-\xbf]|[\xe1\xe3\xe5-\xec\xee]\xb6[\x80-\xbf]|[\xe1\xe4-\xef][\x80-\x82\x86][\x80-\xbf]|[\xe1\xea]\xa1[\x80-\xb7]|[\xe2-\xe9\xeb-\xef]\x9b[\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee\xef][\xa0\xa1\xa3\xa8][\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee][\xa9\xab][\x80-\xbf]|[\xe2-\xec\xee\xef][\xa2\xaa\xb2][\x80-\xbf]|[\xe2-\xec\xee][\xbc-\xbe][\x80-\xbf]|[\xe2-\xef][\x89\x8a\x8d\x8e\x9a\x9c][\x80-\xbf]|[\xe2\xe4-\xef]\x8b[\x80-\xbf]|[\xe3-\xe8\xea-\xec\xee]\xbf[\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee\xef][\x9f\xa7][\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee][\xad\xaf][\x80-\xbf]|[\xe3-\xec\xee\xef][\xa6\xb0\xb1\xb3][\x80-\xbf]|[\xe3-\xec\xee]\xb7[\x80-\xbf]|[\xe3-\xef][\x83\x8c\x8f\x9d][\x80-\xbf]|\xe0\xa0[\x80-\xad\xb0-\xbe]|\xe0\xa1[\x80-\x9b\x9e]|\xe0\xa2[\xa0-\xb4]|\xe0\xa3[\xa3-\xbf]|\xe0\xa6[\x80-\x83\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb6-\xb9\xbc-\xbf]|\xe0\xa7[\x80-\x84\x87\x88\x8b-\x8e\x97\x9c\x9d\x9f-\xa3\xa6-\xbb]|\xe0\xa8[\x81-\x83\x85-\x8a\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5\xb6\xb8\xb9\xbc\xbe\xbf]|\xe0\xa9[\x80-\x82\x87\x88\x8b-\x8d\x91\x99-\x9c\x9e\xa6-\xb5]|\xe0\xaa[\x81-\x83\x85-\x8d\x8f-\x91\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc-\xbf]|\xe0\xab[\x80-\x85\x87-\x89\x8b-\x8d\x90\xa0-\xa3\xa6-\xb1\xb9]|\xe0\xac[\x81-\x83\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc-\xbf]|\xe0\xad[\x80-\x84\x87\x88\x8b-\x8d\x96\x97\x9c\x9d\x9f-\xa3\xa6-\xb7]|\xe0\xae[\x82\x83\x85-\x8a\x8e-\x90\x92-\x95\x99\x9a\x9c\x9e\x9f\xa3\xa4\xa8-\xaa\xae-\xb9\xbe\xbf]|\xe0\xaf[\x80-\x82\x86-\x88\x8a-\x8d\x90\x97\xa6-\xba]|\xe0\xb0[\x80-\x83\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb9\xbd-\xbf]|\xe0\xb1[\x80-\x84\x86-\x88\x8a-\x8d\x95\x96\x98-\x9a\xa0-\xa3\xa6-\xaf\xb8-\xbf]|\xe0\xb2[\x81-\x83\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb3\xb5-\xb9\xbc-\xbf]|\xe0\xb3[\x80-\x84\x86-\x88\x8a-\x8d\x95\x96\x9e\xa0-\xa3\xa6-\xaf\xb1\xb2]|\xe0\xb4[\x81-\x83\x85-\x8c\x8e-\x90\x92-\xba\xbd-\xbf]|\xe0\xb5[\x80-\x84\x86-\x88\x8a-\x8e\x97\x9f-\xa3\xa6-\xb5\xb9-\xbf]|\xe0\xb6[\x82\x83\x85-\x96\x9a-\xb1\xb3-\xbb\xbd]|\xe0\xb7[\x80-\x86\x8a\x8f-\x94\x96\x98-\x9f\xa6-\xaf\xb2-\xb4]|\xe0\xb8[\x81-\xba\xbf]|\xe0\xb9[\x80-\x9b]|\xe0\xba[\x81\x82\x84\x87\x88\x8a\x8d\x94-\x97\x99-\x9f\xa1-\xa3\xa5\xa7\xaa\xab\xad-\xb9\xbb-\xbd]|\xe0\xbb[\x80-\x84\x86\x88-\x8d\x90-\x99\x9c-\x9f]|\xe0\xbc[\x80-\xb9\xbe\xbf]|\xe0\xbd[\x80-\x87\x89-\xac\xb1-\xbf]|\xe0\xbe[\x80-\x97\x99-\xbc\xbe\xbf]|\xe0\xbf[\x80-\x8c\x8e-\x9a]|\xe1\x83[\x80-\x85\x87\x8d\x90-\xbf]|\xe1\x89[\x80-\x88\x8a-\x8d\x90-\x96\x98\x9a-\x9d\xa0-\xbf]|\xe1\x8a[\x80-\x88\x8a-\x8d\x90-\xb0\xb2-\xb5\xb8-\xbe]|\xe1\x8b[\x80\x82-\x85\x88-\x96\x98-\xbf]|\xe1\x8c[\x80-\x90\x92-\x95\x98-\xbf]|\xe1\x8d[\x80-\x9a\x9d-\xbc]|\xe1\x8e[\x80-\x99\xa0-\xbf]|\xe1\x8f[\x80-\xb5\xb8-\xbd]|\xe1\x9a[\x81-\x9a\xa0-\xbf]|\xe1\x9b[\x80-\xb8]|\xe1\x9c[\x80-\x8c\x8e-\x94\xa0-\xb6]|\xe1\x9d[\x80-\x93\xa0-\xac\xae-\xb0\xb2\xb3]|\xe1\x9f[\x80-\x9d\xa0-\xa9\xb0-\xb9]|\xe1\xa0[\x80-\x8d\x90-\x99\xa0-\xbf]|\xe1\xa2[\x80-\xaa\xb0-\xbf]|\xe1\xa3[\x80-\xb5]|\xe1\xa4[\x80-\x9e\xa0-\xab\xb0-\xbb]|\xe1\xa5[\x80\x84-\xad\xb0-\xb4]|\xe1\xa6[\x80-\xab\xb0-\xbf]|\xe1\xa7[\x80-\x89\x90-\x9a\x9e-\xbf]|\xe1\xa8[\x80-\x9b\x9e-\xbf]|\xe1\xa9[\x80-\x9e\xa0-\xbc\xbf]|\xe1\xaa[\x80-\x89\x90-\x99\xa0-\xad\xb0-\xbe]|\xe1\xad[\x80-\x8b\x90-\xbc]|\xe1\xaf[\x80-\xb3\xbc-\xbf]|\xe1\xb0[\x80-\xb7\xbb-\xbf]|\xe1\xb1[\x80-\x89\x8d-\xbf]|\xe1\xb3[\x80-\x87\x90-\xb6\xb8\xb9]|\xe1\xb7[\x80-\xb5\xbc-\xbf]|\xe1\xbc[\x80-\x95\x98-\x9d\xa0-\xbf]|\xe1\xbd[\x80-\x85\x88-\x8d\x90-\x97\x99\x9b\x9d\x9f-\xbd]|\xe1\xbe[\x80-\xb4\xb6-\xbf]|\xe1\xbf[\x80-\x84\x86-\x93\x96-\x9b\x9d-\xaf\xb2-\xb4\xb6-\xbe]|\xe2\x80[\x90-\x97\xa0-\xa7\xb0-\xb8\xbb-\xbf]|\xe2\x81[\x80-\x84\x87-\x9e\xb0\xb1\xb4-\xbc\xbf]|\xe2\x82[\x80-\x8c\x90-\x9c\xa0-\xbe]|\xe2\x83[\x90-\xb0]|\xe2\x86[\x80-\x8b\x90-\xbf]|\xe2\x8c[\x80-\x87\x8c-\xa8\xab-\xbf]|\xe2\x8f[\x80-\xba]|\xe2\x90[\x80-\xa6]|\xe2\x91[\x80-\x8a\xa0-\xbf]|\xe2\x9d[\x80-\xa7\xb6-\xbf]|\xe2\x9f[\x80-\x84\x87-\xa5\xb0-\xbf]|\xe2\xa6[\x80-\x82\x99-\xbf]|\xe2\xa7[\x80-\x97\x9c-\xbb\xbe\xbf]|\xe2\xad[\x80-\xb3\xb6-\xbf]|\xe2\xae[\x80-\x95\x98-\xb9\xbd-\xbf]|\xe2\xaf[\x80-\x88\x8a-\x91\xac-\xaf]|\xe2\xb0[\x80-\xae\xb0-\xbf]|\xe2\xb1[\x80-\x9e\xa0-\xbf]|\xe2\xb3[\x80-\xb3\xb9-\xbf]|\xe2\xb4[\x80-\xa5\xa7\xad\xb0-\xbf]|\xe2\xb5[\x80-\xa7\xaf\xb0\xbf]|\xe2\xb6[\x80-\x96\xa0-\xa6\xa8-\xae\xb0-\xb6\xb8-\xbe]|\xe2\xb7[\x80-\x86\x88-\x8e\x90-\x96\x98-\x9e\xa0-\xbf]|\xe2\xb8[\x80\x81\x86-\x88\x8b\x8e-\x9b\x9e\x9f\xaa-\xbf]|\xe2\xb9[\x80\x81]|\xe2\xba[\x80-\x99\x9b-\xbf]|\xe2\xbb[\x80-\xb3]|\xe2\xbf[\x80-\x95\xb0-\xbb]|\xe3\x80[\x81-\x87\x92\x93\x9c\xa0-\xbf]|\xe3\x81[\x81-\xbf]|\xe3\x82[\x80-\x96\x99-\xbf]|\xe3\x84[\x85-\xad\xb1-\xbf]|\xe3\x86[\x80-\x8e\x90-\xba]|\xe3\x87[\x80-\xa3\xb0-\xbf]|\xe3\x88[\x80-\x9e\xa0-\xbf]|\xe3\x8b[\x80-\xbe]|\xe4\xb6[\x80-\xb5]|\xe9\xbf[\x80-\x95]|\xea\x92[\x80-\x8c\x90-\xbf]|\xea\x93[\x80-\x86\x90-\xbf]|\xea\x98[\x80-\xab]|\xea\x9b[\x80-\xb7]|\xea\x9e[\x80-\xad\xb0-\xb7]|\xea\x9f[\xb7-\xbf]|\xea\xa0[\x80-\xab\xb0-\xb9]|\xea\xa3[\x80-\x84\x8e-\x99\xa0-\xbd]|\xea\xa5[\x80-\x93\x9f-\xbc]|\xea\xa7[\x80-\x8d\x8f-\x99\x9e-\xbe]|\xea\xa8[\x80-\xb6]|\xea\xa9[\x80-\x8d\x90-\x99\x9c-\xbf]|\xea\xab[\x80-\x82\x9b-\xb6]|\xea\xac[\x81-\x86\x89-\x8e\x91-\x96\xa0-\xa6\xa8-\xae\xb0-\xbf]|\xea\xad[\x80-\xa5\xb0-\xbf]|\xea\xaf[\x80-\xad\xb0-\xb9]|\xed\x9e[\x80-\xa3\xb0-\xbf]|\xed\x9f[\x80-\x86\x8b-\xbb]|\xef\xa9[\x80-\xad\xb0-\xbf]|\xef\xab[\x80-\x99]|\xef\xac[\x80-\x86\x93-\x97\x9d-\xb6\xb8-\xbc\xbe]|\xef\xad[\x80\x81\x83\x84\x86-\xbf]|\xef\xaf[\x80\x81\x93-\xbf]|\xef\xb4[\x80-\xbd]|\xef\xb5[\x90-\xbf]|\xef\xb6[\x80-\x8f\x92-\xbf]|\xef\xb7[\x80-\x87\xb0-\xbd]|\xef\xb8[\x80-\x96\x99\xa0-\xb4]|\xef\xb9[\x85\x86\x89-\x92\x94-\x98\x9f-\xa6\xa8-\xab\xb0-\xb4\xb6-\xbf]|\xef\xbb[\x80-\xbc]|\xef\xbc[\x81-\x87\x8a-\xba\xbc\xbe\xbf]|\xef\xbd[\x80-\x9a\x9c\x9e\xa1\xa4-\xbf]|\xef\xbe[\x80-\xbe]|\xef\xbf[\x82-\x87\x8a-\x8f\x92-\x97\x9a-\x9c\xa0-\xa6\xa8-\xae\xbc\xbd])|([\xf0\xf3]\xa0[\x84-\x86][\x80-\xbf]|\xf0[\x90\x91\x94\x9d\x9f-\xac]\x98[\x80-\xbf]|\xf0[\x90\x92\x93\x9d\xa0-\xac]\x82[\x80-\xbf]|\xf0[\x90\x92\x93\xa1-\xac]\x85[\x80-\xbf]|\xf0[\x90\x92\x94\x9d\x9f-\xac]\x90[\x80-\xbf]|\xf0[\x90\x94\x9f-\xac]\x91[\x80-\xbf]|\xf0[\x90\x9b\xa0-\xac]\xb0[\x80-\xbf]|\xf0[\x90\x9d\x9f-\xac]\x99[\x80-\xbf]|\xf0[\x90\x9d\xa0-\xa9\xab\xac]\x9b[\x80-\xbf]|\xf0[\x90\x9f-\xac]\x9a[\x80-\xbf]|\xf0[\x91-\x93\x9d\xa0-\xac]\x80[\x80-\xbf]|\xf0[\x91-\x93\x9d\xa1-\xac]\x86[\x80-\xbf]|\xf0[\x91\x92\x94\x9f-\xac]\x92[\x80-\xbf]|\xf0[\x92\x93\x9d\x9f-\xac][\x81\x8c][\x80-\xbf]|\xf0[\x92\x93\x9d\xa0-\xac]\x88[\x80-\xbf]|\xf0[\x92\x93\x9f-\xac]\x8d[\x80-\xbf]|\xf0[\x92\x93\xa0-\xac][\x83\x87\x89-\x8b][\x80-\xbf]|\xf0[\x92\x93\xa1-\xac]\x84[\x80-\xbf]|\xf0[\x92\x94\x9f-\xac][\x93\x94][\x80-\xbf]|\xf0[\x93\x9f-\xac][\x8e\x8f][\x80-\xbf]|\xf0[\x94\x9d\x9f-\xac]\x97[\x80-\xbf]|\xf0[\x94\x9d\xa0-\xac]\x96[\x80-\xbf]|\xf0[\x94\xa0-\xac]\x95[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xaa\xac\xaf]\xa0[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xac\xaf][\xa1\xa2][\x80-\xbf]|\xf0[\x96\x9d\xa0-\xac\xaf][\xa3-\xa7][\x80-\xbf]|\xf0[\x96\xa0-\xab]\xbc[\x80-\xbf]|\xf0[\x96\xa0-\xac]\xac[\x80-\xbf]|\xf0[\x9d\x9f-\xaa\xac]\x9c[\x80-\xbf]|\xf0[\x9d\x9f-\xac]\x9e[\x80-\xbf]|\xf0[\x9d\xa0-\xac][\x9d\xa8\xa9][\x80-\xbf]|\xf0[\xa0-\xab][\xba\xbb\xbd-\xbf][\x80-\xbf]|\xf0[\xa0-\xac][\x9f\xaa\xab\xad-\xaf\xb1-\xb9][\x80-\xbf]|\xf0\x90\x80[\x80-\x8b\x8d-\xa6\xa8-\xba\xbc\xbd\xbf]|\xf0\x90\x81[\x80-\x8d\x90-\x9d]|\xf0\x90\x83[\x80-\xba]|\xf0\x90\x84[\x80-\x82\x87-\xb3\xb7-\xbf]|\xf0\x90\x86[\x80-\x8c\x90-\x9b\xa0]|\xf0\x90\x87[\x90-\xbd]|\xf0\x90\x8a[\x80-\x9c\xa0-\xbf]|\xf0\x90\x8b[\x80-\x90\xa0-\xbb]|\xf0\x90\x8c[\x80-\xa3\xb0-\xbf]|\xf0\x90\x8d[\x80-\x8a\x90-\xba]|\xf0\x90\x8e[\x80-\x9d\x9f-\xbf]|\xf0\x90\x8f[\x80-\x83\x88-\x95]|\xf0\x90\x92[\x80-\x9d\xa0-\xa9]|\xf0\x90\x94[\x80-\xa7\xb0-\xbf]|\xf0\x90\x95[\x80-\xa3\xaf]|\xf0\x90\x9c[\x80-\xb6]|\xf0\x90\x9d[\x80-\x95\xa0-\xa7]|\xf0\x90\xa0[\x80-\x85\x88\x8a-\xb5\xb7\xb8\xbc\xbf]|\xf0\x90\xa1[\x80-\x95\x97-\xbf]|\xf0\x90\xa2[\x80-\x9e\xa7-\xaf]|\xf0\x90\xa3[\xa0-\xb2\xb4\xb5\xbb-\xbf]|\xf0\x90\xa4[\x80-\x9b\x9f-\xb9\xbf]|\xf0\x90\xa6[\x80-\xb7\xbc-\xbf]|\xf0\x90\xa7[\x80-\x8f\x92-\xbf]|\xf0\x90\xa8[\x80-\x83\x85\x86\x8c-\x93\x95-\x97\x99-\xb3\xb8-\xba\xbf]|\xf0\x90\xa9[\x80-\x87\x90-\x98\xa0-\xbf]|\xf0\x90\xaa[\x80-\x9f]|\xf0\x90\xab[\x80-\xa6\xab-\xb6]|\xf0\x90\xac[\x80-\xb5\xb9-\xbf]|\xf0\x90\xad[\x80-\x95\x98-\xb2\xb8-\xbf]|\xf0\x90\xae[\x80-\x91\x99-\x9c\xa9-\xaf]|\xf0\x90\xb1[\x80-\x88]|\xf0\x90\xb2[\x80-\xb2]|\xf0\x90\xb3[\x80-\xb2\xba-\xbf]|\xf0\x90\xb9[\xa0-\xbe]|\xf0\x91\x81[\x80-\x8d\x92-\xaf\xbf]|\xf0\x91\x82[\x80-\xbc\xbe\xbf]|\xf0\x91\x83[\x80\x81\x90-\xa8\xb0-\xb9]|\xf0\x91\x84[\x80-\xb4\xb6-\xbf]|\xf0\x91\x85[\x80-\x83\x90-\xb6]|\xf0\x91\x87[\x80-\x8d\x90-\x9f\xa1-\xb4]|\xf0\x91\x88[\x80-\x91\x93-\xbd]|\xf0\x91\x8a[\x80-\x86\x88\x8a-\x8d\x8f-\x9d\x9f-\xa9\xb0-\xbf]|\xf0\x91\x8b[\x80-\xaa\xb0-\xb9]|\xf0\x91\x8c[\x80-\x83\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc-\xbf]|\xf0\x91\x8d[\x80-\x84\x87\x88\x8b-\x8d\x90\x97\x9d-\xa3\xa6-\xac\xb0-\xb4]|\xf0\x91\x93[\x80-\x87\x90-\x99]|\xf0\x91\x96[\x80-\xb5\xb8-\xbf]|\xf0\x91\x97[\x80-\x9d]|\xf0\x91\x99[\x80-\x84\x90-\x99]|\xf0\x91\x9a[\x80-\xb7]|\xf0\x91\x9b[\x80-\x89]|\xf0\x91\x9c[\x80-\x99\x9d-\xab\xb0-\xbf]|\xf0\x91\xa2[\xa0-\xbf]|\xf0\x91\xa3[\x80-\xb2\xbf]|\xf0\x91\xab[\x80-\xb8]|\xf0\x92\x8e[\x80-\x99]|\xf0\x92\x91[\x80-\xae\xb0-\xb4]|\xf0\x92\x95[\x80-\x83]|\xf0\x93\x90[\x80-\xae]|\xf0\x94\x99[\x80-\x86]|\xf0\x96\xa8[\x80-\xb8]|\xf0\x96\xa9[\x80-\x9e\xa0-\xa9\xae\xaf]|\xf0\x96\xab[\x90-\xad\xb0-\xb5]|\xf0\x96\xad[\x80-\x85\x90-\x99\x9b-\xa1\xa3-\xb7\xbd-\xbf]|\xf0\x96\xae[\x80-\x8f]|\xf0\x96\xbd[\x80-\x84\x90-\xbe]|\xf0\x96\xbe[\x8f-\x9f]|\xf0\x9b\x80[\x80\x81]|\xf0\x9b\xb1[\x80-\xaa\xb0-\xbc]|\xf0\x9b\xb2[\x80-\x88\x90-\x99\x9c-\x9f]|\xf0\x9d\x83[\x80-\xb5]|\xf0\x9d\x84[\x80-\xa6\xa9-\xbf]|\xf0\x9d\x85[\x80-\xb2\xbb-\xbf]|\xf0\x9d\x87[\x80-\xa8]|\xf0\x9d\x89[\x80-\x85]|\xf0\x9d\x8d[\x80-\x96\xa0-\xb1]|\xf0\x9d\x91[\x80-\x94\x96-\xbf]|\xf0\x9d\x92[\x80-\x9c\x9e\x9f\xa2\xa5\xa6\xa9-\xac\xae-\xb9\xbb\xbd-\xbf]|\xf0\x9d\x93[\x80-\x83\x85-\xbf]|\xf0\x9d\x94[\x80-\x85\x87-\x8a\x8d-\x94\x96-\x9c\x9e-\xb9\xbb-\xbe]|\xf0\x9d\x95[\x80-\x84\x86\x8a-\x90\x92-\xbf]|\xf0\x9d\x9a[\x80-\xa5\xa8-\xbf]|\xf0\x9d\x9f[\x80-\x8b\x8e-\xbf]|\xf0\x9d\xaa[\x80-\x8b\x9b-\x9f\xa1-\xaf]|\xf0\x9e\xa3[\x80-\x84\x87-\x96]|\xf0\x9e\xb8[\x80-\x83\x85-\x9f\xa1\xa2\xa4\xa7\xa9-\xb2\xb4-\xb7\xb9\xbb]|\xf0\x9e\xb9[\x82\x87\x89\x8b\x8d-\x8f\x91\x92\x94\x97\x99\x9b\x9d\x9f\xa1\xa2\xa4\xa7-\xaa\xac-\xb2\xb4-\xb7\xb9-\xbc\xbe]|\xf0\x9e\xba[\x80-\x89\x8b-\x9b\xa1-\xa3\xa5-\xa9\xab-\xbb]|\xf0\x9e\xbb[\xb0\xb1]|\xf0\x9f[\x80\x85][\x80-\xab\xb0-\xbf]|\xf0\x9f\x82[\x80-\x93\xa0-\xae\xb1-\xbf]|\xf0\x9f\x83[\x81-\x8f\x91-\xb5]|\xf0\x9f\x84[\x80-\x8c\x90-\xae\xb0-\xbf]|\xf0\x9f\x86[\x80-\x9a]|\xf0\x9f\x87[\xa6-\xbf]|\xf0\x9f\x88[\x80-\x82\x90-\xba]|\xf0\x9f\x89[\x80-\x88\x90\x91]|\xf0\x9f\x95[\x80-\xb9\xbb-\xbf]|\xf0\x9f\x96[\x80-\xa3\xa5-\xbf]|\xf0\x9f\x9b[\x80-\x90\xa0-\xac\xb0-\xb3]|\xf0\x9f\x9d[\x80-\xb3]|\xf0\x9f\x9f[\x80-\x94]|\xf0\x9f\xa0[\x80-\x8b\x90-\xbf]|\xf0\x9f\xa1[\x80-\x87\x90-\x99\xa0-\xbf]|\xf0\x9f\xa2[\x80-\x87\x90-\xad]|\xf0\x9f\xa4[\x90-\x98]|\xf0\x9f\xa6[\x80-\x84]|\xf0\x9f\xa7\x80|\xf0\xaa\x9b[\x80-\x96]|\xf0\xab\x9c[\x80-\xb4]|\xf0\xab\xa0[\x80-\x9d\xa0-\xbf]|\xf0\xac\xba[\x80-\xa1]|\xf0\xaf\xa8[\x80-\x9d]|\xf3[\xb0-\xbe]\xbf[\x80-\xbf]|\xf3[\xb0-\xbf][\x80-\xbe][\x80-\xbf]|\xf3\xa0\x87[\x80-\xaf]|\xf3\xbf\xbf[\x80-\xbd]|\xf4[\x80-\x8e]\xbf[\x80-\xbf]|\xf4[\x80-\x8f][\x80-\xbe][\x80-\xbf]|\xf4\x8f\xbf[\x80-\xbd])|(\xe2\x80\x8c)|(\xe2\x80\x8d))* { 
  yylval.obj = identifier_new(yytext);
  return TOKEN_IDENTIFIER;
}
 /* |⟨initial⟩ ⟨subsequent⟩*| */
\|([!$%&\*\/:<=>\?^_~a-zA-Z]|([\xc3-\xcc\xcf-\xd1\xd3\xda][\x80-\xbf]|\xc2[\xa1-\xaa\xac\xae-\xba\xbc-\xbf]|\xcd[\x80-\xb7\xba-\xbf]|\xce[\x84-\x8a\x8c\x8e-\xa1\xa3-\xbf]|\xd2[\x80-\x87\x8a-\xbf]|\xd4[\x80-\xaf\xb1-\xbf]|\xd5[\x80-\x96\x99-\x9f\xa1-\xbf]|\xd6[\x80-\x87\x89\x8a\x8d-\x8f\x91-\xbf]|\xd7[\x80-\x87\x90-\xaa\xb0-\xb4]|\xd8[\x86-\x9b\x9e-\xbf]|\xd9[\x80-\x9f\xaa-\xbf]|\xdb[\x80-\x9c\x9e-\xaf\xba-\xbf]|\xdc[\x80-\x8d\x90-\xbf]|\xdd[\x80-\x8a\x8d-\xbf]|\xde[\x80-\xb1]|\xdf[\x8a-\xba])|([\xe1-\xe9\xeb-\xef][\x92\x93\x98\x99][\x80-\xbf]|[\xe1-\xef][\x85\x94-\x97][\x80-\xbf]|[\xe1\xe2\xe4-\xef][\x84\x87\x88][\x80-\xbf]|[\xe1\xe3-\xec\xee\xef]\xba[\x80-\xbf]|[\xe1\xe3-\xec\xee][\xb4\xb5\xb8\xb9\xbb][\x80-\xbf]|[\xe1\xe3-\xef][\x90\x91][\x80-\xbf]|[\xe1\xe3\xe5-\xec\xee]\xb6[\x80-\xbf]|[\xe1\xe4-\xef]\x86[\x80-\xbf]|[\xe1\xea]\xa1[\x80-\xb7]|[\xe2-\xe9\xeb-\xef]\x9b[\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee\xef][\x9e\xa0-\xa5\xa8][\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee][\xa9\xab\xac][\x80-\xbf]|[\xe2-\xec\xee\xef][\xaa\xb2][\x80-\xbf]|[\xe2-\xec\xee][\xbc-\xbe][\x80-\xbf]|[\xe2-\xef][\x89\x8a\x8d\x8e\x9a\x9c][\x80-\xbf]|[\xe2\xe4-\xef]\x8b[\x80-\xbf]|[\xe3-\xe8\xea-\xec\xee]\xbf[\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee\xef][\x9f\xa6\xa7][\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee][\xad\xaf][\x80-\xbf]|[\xe3-\xec\xee\xef][\xae\xb0\xb1\xb3][\x80-\xbf]|[\xe3-\xec\xee]\xb7[\x80-\xbf]|[\xe3-\xef][\x83\x8c\x8f\x9d][\x80-\xbf]|[\xe4-\xef][\x80-\x82][\x80-\xbf]|\xe0\xa0[\x80-\xad\xb0-\xbe]|\xe0\xa1[\x80-\x9b\x9e]|\xe0\xa2[\xa0-\xb4]|\xe0\xa3[\xa3-\xbf]|\xe0\xa4[\x80-\x82\x84-\xba\xbc\xbd]|\xe0\xa5[\x81-\x88\x8d\x90-\xa5\xb0-\xbf]|\xe0\xa6[\x80\x81\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb6-\xb9\xbc\xbd]|\xe0\xa7[\x81-\x84\x8d\x8e\x9c\x9d\x9f-\xa3\xb0-\xbb]|\xe0\xa8[\x81\x82\x85-\x8a\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5\xb6\xb8\xb9\xbc]|\xe0\xa9[\x81\x82\x87\x88\x8b-\x8d\x91\x99-\x9c\x9e\xb0-\xb5]|\xe0\xaa[\x81\x82\x85-\x8d\x8f-\x91\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc\xbd]|\xe0\xab[\x81-\x85\x87\x88\x8d\x90\xa0-\xa3\xb0\xb1\xb9]|\xe0\xac[\x81\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc\xbd\xbf]|\xe0\xad[\x81-\x84\x8d\x96\x9c\x9d\x9f-\xa3\xb0-\xb7]|\xe0\xae[\x82\x83\x85-\x8a\x8e-\x90\x92-\x95\x99\x9a\x9c\x9e\x9f\xa3\xa4\xa8-\xaa\xae-\xb9]|\xe0\xaf[\x80\x8d\x90\xb0-\xba]|\xe0\xb0[\x80\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb9\xbd-\xbf]|\xe0\xb1[\x80\x86-\x88\x8a-\x8d\x95\x96\x98-\x9a\xa0-\xa3\xb8-\xbf]|\xe0\xb2[\x81\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb3\xb5-\xb9\xbc\xbd\xbf]|\xe0\xb3[\x86\x8c\x8d\x9e\xa0-\xa3\xb1\xb2]|\xe0\xb4[\x81\x85-\x8c\x8e-\x90\x92-\xba\xbd]|\xe0\xb5[\x81-\x84\x8d\x8e\x9f-\xa3\xb0-\xb5\xb9-\xbf]|\xe0\xb6[\x85-\x96\x9a-\xb1\xb3-\xbb\xbd]|\xe0\xb7[\x80-\x86\x8a\x92-\x94\x96\xb4]|\xe0\xb8[\x81-\xba\xbf]|\xe0\xb9[\x80-\x8f\x9a\x9b]|\xe0\xba[\x81\x82\x84\x87\x88\x8a\x8d\x94-\x97\x99-\x9f\xa1-\xa3\xa5\xa7\xaa\xab\xad-\xb9\xbb-\xbd]|\xe0\xbb[\x80-\x84\x86\x88-\x8d\x9c-\x9f]|\xe0\xbc[\x80-\x9f\xaa-\xb9]|\xe0\xbd[\x80-\x87\x89-\xac\xb1-\xbe]|\xe0\xbe[\x80-\x97\x99-\xbc\xbe\xbf]|\xe0\xbf[\x80-\x8c\x8e-\x9a]|\xe1\x80[\x80-\xaa\xad-\xb0\xb2-\xb7\xb9\xba\xbd-\xbf]|\xe1\x81[\x8a-\x95\x98-\xa1\xa5\xa6\xae-\xbf]|\xe1\x82[\x80-\x82\x85\x86\x8d\x8e\x9d-\xbf]|\xe1\x83[\x80-\x85\x87\x8d\x90-\xbf]|\xe1\x89[\x80-\x88\x8a-\x8d\x90-\x96\x98\x9a-\x9d\xa0-\xbf]|\xe1\x8a[\x80-\x88\x8a-\x8d\x90-\xb0\xb2-\xb5\xb8-\xbe]|\xe1\x8b[\x80\x82-\x85\x88-\x96\x98-\xbf]|\xe1\x8c[\x80-\x90\x92-\x95\x98-\xbf]|\xe1\x8d[\x80-\x9a\x9d-\xbc]|\xe1\x8e[\x80-\x99\xa0-\xbf]|\xe1\x8f[\x80-\xb5\xb8-\xbd]|\xe1\x9a[\x81-\x9a\xa0-\xbf]|\xe1\x9b[\x80-\xb8]|\xe1\x9c[\x80-\x8c\x8e-\x94\xa0-\xb6]|\xe1\x9d[\x80-\x93\xa0-\xac\xae-\xb0\xb2\xb3]|\xe1\x9e[\x80-\xb5\xb7-\xbd]|\xe1\x9f[\x86\x89-\x9d\xb0-\xb9]|\xe1\xa0[\x80-\x8d\xa0-\xbf]|\xe1\xa2[\x80-\xaa\xb0-\xbf]|\xe1\xa3[\x80-\xb5]|\xe1\xa4[\x80-\x9e\xa0-\xa2\xa7\xa8\xb2\xb9-\xbb]|\xe1\xa5[\x80\x84\x85\x90-\xad\xb0-\xb4]|\xe1\xa6[\x80-\xab\xb0-\xbf]|\xe1\xa7[\x80-\x89\x9a\x9e-\xbf]|\xe1\xa8[\x80-\x98\x9b\x9e-\xbf]|\xe1\xa9[\x80-\x94\x96\x98-\x9e\xa0\xa2\xa5-\xac\xb3-\xbc\xbf]|\xe1\xaa[\xa0-\xad\xb0-\xbd]|\xe1\xac[\x80-\x83\x85-\xb4\xb6-\xba\xbc]|\xe1\xad[\x82\x85-\x8b\x9a-\xbc]|\xe1\xae[\x80\x81\x83-\xa0\xa2-\xa5\xa8\xa9\xab-\xaf\xba-\xbf]|\xe1\xaf[\x80-\xa6\xa8\xa9\xad\xaf-\xb1\xbc-\xbf]|\xe1\xb0[\x80-\xa3\xac-\xb3\xb6\xb7\xbb-\xbf]|\xe1\xb1[\x8d-\x8f\x9a-\xbf]|\xe1\xb3[\x80-\x87\x90-\xa0\xa2-\xb1\xb4-\xb6\xb8\xb9]|\xe1\xb7[\x80-\xb5\xbc-\xbf]|\xe1\xbc[\x80-\x95\x98-\x9d\xa0-\xbf]|\xe1\xbd[\x80-\x85\x88-\x8d\x90-\x97\x99\x9b\x9d\x9f-\xbd]|\xe1\xbe[\x80-\xb4\xb6-\xbf]|\xe1\xbf[\x80-\x84\x86-\x93\x96-\x9b\x9d-\xaf\xb2-\xb4\xb6-\xbe]|\xe2\x80[\x90-\x97\xa0-\xa7\xb0-\xb8\xbb-\xbf]|\xe2\x81[\x80-\x84\x87-\x9e\xb0\xb1\xb4-\xbc\xbf]|\xe2\x82[\x80-\x8c\x90-\x9c\xa0-\xbe]|\xe2\x83[\x90-\x9c\xa1\xa5-\xb0]|\xe2\x86[\x80-\x8b\x90-\xbf]|\xe2\x8c[\x80-\x87\x8c-\xa8\xab-\xbf]|\xe2\x8f[\x80-\xba]|\xe2\x90[\x80-\xa6]|\xe2\x91[\x80-\x8a\xa0-\xbf]|\xe2\x9d[\x80-\xa7\xb6-\xbf]|\xe2\x9f[\x80-\x84\x87-\xa5\xb0-\xbf]|\xe2\xa6[\x80-\x82\x99-\xbf]|\xe2\xa7[\x80-\x97\x9c-\xbb\xbe\xbf]|\xe2\xad[\x80-\xb3\xb6-\xbf]|\xe2\xae[\x80-\x95\x98-\xb9\xbd-\xbf]|\xe2\xaf[\x80-\x88\x8a-\x91\xac-\xaf]|\xe2\xb0[\x80-\xae\xb0-\xbf]|\xe2\xb1[\x80-\x9e\xa0-\xbf]|\xe2\xb3[\x80-\xb3\xb9-\xbf]|\xe2\xb4[\x80-\xa5\xa7\xad\xb0-\xbf]|\xe2\xb5[\x80-\xa7\xaf\xb0\xbf]|\xe2\xb6[\x80-\x96\xa0-\xa6\xa8-\xae\xb0-\xb6\xb8-\xbe]|\xe2\xb7[\x80-\x86\x88-\x8e\x90-\x96\x98-\x9e\xa0-\xbf]|\xe2\xb8[\x80\x81\x86-\x88\x8b\x8e-\x9b\x9e\x9f\xaa-\xbf]|\xe2\xb9[\x80\x81]|\xe2\xba[\x80-\x99\x9b-\xbf]|\xe2\xbb[\x80-\xb3]|\xe2\xbf[\x80-\x95\xb0-\xbb]|\xe3\x80[\x81-\x87\x92\x93\x9c\xa0-\xad\xb0-\xbf]|\xe3\x81[\x81-\xbf]|\xe3\x82[\x80-\x96\x99-\xbf]|\xe3\x84[\x85-\xad\xb1-\xbf]|\xe3\x86[\x80-\x8e\x90-\xba]|\xe3\x87[\x80-\xa3\xb0-\xbf]|\xe3\x88[\x80-\x9e\xa0-\xbf]|\xe3\x8b[\x80-\xbe]|\xe4\xb6[\x80-\xb5]|\xe9\xbf[\x80-\x95]|\xea\x92[\x80-\x8c\x90-\xbf]|\xea\x93[\x80-\x86\x90-\xbf]|\xea\x98[\x80-\x9f\xaa\xab]|\xea\x99[\x80-\xaf\xb3-\xbf]|\xea\x9b[\x80-\xb7]|\xea\x9e[\x80-\xad\xb0-\xb7]|\xea\x9f[\xb7-\xbf]|\xea\xa0[\x80-\xa2\xa5\xa6\xa8-\xab\xb0-\xb9]|\xea\xa2[\x82-\xb3]|\xea\xa3[\x84\x8e\x8f\xa0-\xbd]|\xea\xa4[\x8a-\xbf]|\xea\xa5[\x80-\x91\x9f-\xbc]|\xea\xa6[\x80-\x82\x84-\xb3\xb6-\xb9\xbc]|\xea\xa7[\x81-\x8d\x8f\x9e-\xaf\xba-\xbe]|\xea\xa8[\x80-\xae\xb1\xb2\xb5\xb6]|\xea\xa9[\x80-\x8c\x9c-\xba\xbc\xbe\xbf]|\xea\xab[\x80-\x82\x9b-\xaa\xac\xad\xb0-\xb4\xb6]|\xea\xac[\x81-\x86\x89-\x8e\x91-\x96\xa0-\xa6\xa8-\xae\xb0-\xbf]|\xea\xad[\x80-\xa5\xb0-\xbf]|\xea\xaf[\x80-\xa2\xa5\xa8\xab\xad]|\xed\x9e[\x80-\xa3\xb0-\xbf]|\xed\x9f[\x80-\x86\x8b-\xbb]|\xef\xa9[\x80-\xad\xb0-\xbf]|\xef\xab[\x80-\x99]|\xef\xac[\x80-\x86\x93-\x97\x9d-\xb6\xb8-\xbc\xbe]|\xef\xad[\x80\x81\x83\x84\x86-\xbf]|\xef\xaf[\x80\x81\x93-\xbf]|\xef\xb4[\x80-\xbd]|\xef\xb5[\x90-\xbf]|\xef\xb6[\x80-\x8f\x92-\xbf]|\xef\xb7[\x80-\x87\xb0-\xbd]|\xef\xb8[\x80-\x96\x99\xa0-\xb4]|\xef\xb9[\x85\x86\x89-\x92\x94-\x98\x9f-\xa6\xa8-\xab\xb0-\xb4\xb6-\xbf]|\xef\xbb[\x80-\xbc]|\xef\xbc[\x81-\x87\x8a-\x8f\x9a-\xba\xbc\xbe\xbf]|\xef\xbd[\x80-\x9a\x9c\x9e\xa1\xa4-\xbf]|\xef\xbe[\x80-\xbe]|\xef\xbf[\x82-\x87\x8a-\x8f\x92-\x97\x9a-\x9c\xa0-\xa6\xa8-\xae\xbc\xbd])|([\xf0\xf3]\xa0[\x84-\x86][\x80-\xbf]|\xf0[\x90\x92\x93\x9d\xa0-\xac]\x82[\x80-\xbf]|\xf0[\x90\x92\x93\xa1-\xac]\x85[\x80-\xbf]|\xf0[\x90\x92\x94\x9d\x9f-\xac]\x90[\x80-\xbf]|\xf0[\x90\x94\x9d\x9f-\xac]\x98[\x80-\xbf]|\xf0[\x90\x94\x9f-\xac]\x91[\x80-\xbf]|\xf0[\x90\x9b\xa0-\xac]\xb0[\x80-\xbf]|\xf0[\x90\x9d\x9f-\xac]\x99[\x80-\xbf]|\xf0[\x90\x9d\xa0-\xa9\xab\xac]\x9b[\x80-\xbf]|\xf0[\x90\x9f-\xac]\x9a[\x80-\xbf]|\xf0[\x92\x93\x9d\x9f-\xac][\x81\x8c][\x80-\xbf]|\xf0[\x92\x93\x9d\xa0-\xac][\x80\x88][\x80-\xbf]|\xf0[\x92\x93\x9d\xa1-\xac]\x86[\x80-\xbf]|\xf0[\x92\x93\x9f-\xac]\x8d[\x80-\xbf]|\xf0[\x92\x93\xa0-\xac][\x83\x87\x89-\x8b][\x80-\xbf]|\xf0[\x92\x93\xa1-\xac]\x84[\x80-\xbf]|\xf0[\x92\x94\x9f-\xac][\x92-\x94][\x80-\xbf]|\xf0[\x93\x9f-\xac][\x8e\x8f][\x80-\xbf]|\xf0[\x94\x9d\x9f-\xac]\x97[\x80-\xbf]|\xf0[\x94\x9d\xa0-\xac]\x96[\x80-\xbf]|\xf0[\x94\xa0-\xac]\x95[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xaa\xac\xaf]\xa0[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xac\xaf][\xa1\xa2][\x80-\xbf]|\xf0[\x96\x9d\xa0-\xac\xaf][\xa3-\xa7][\x80-\xbf]|\xf0[\x96\xa0-\xab]\xbc[\x80-\xbf]|\xf0[\x96\xa0-\xac]\xac[\x80-\xbf]|\xf0[\x9d\x9f-\xaa\xac]\x9c[\x80-\xbf]|\xf0[\x9d\x9f-\xac]\x9e[\x80-\xbf]|\xf0[\x9d\xa0-\xac][\x9d\xa8\xa9][\x80-\xbf]|\xf0[\xa0-\xab][\xba\xbb\xbd-\xbf][\x80-\xbf]|\xf0[\xa0-\xac][\x9f\xaa\xab\xad-\xaf\xb1-\xb9][\x80-\xbf]|\xf0\x90\x80[\x80-\x8b\x8d-\xa6\xa8-\xba\xbc\xbd\xbf]|\xf0\x90\x81[\x80-\x8d\x90-\x9d]|\xf0\x90\x83[\x80-\xba]|\xf0\x90\x84[\x80-\x82\x87-\xb3\xb7-\xbf]|\xf0\x90\x86[\x80-\x8c\x90-\x9b\xa0]|\xf0\x90\x87[\x90-\xbd]|\xf0\x90\x8a[\x80-\x9c\xa0-\xbf]|\xf0\x90\x8b[\x80-\x90\xa0-\xbb]|\xf0\x90\x8c[\x80-\xa3\xb0-\xbf]|\xf0\x90\x8d[\x80-\x8a\x90-\xba]|\xf0\x90\x8e[\x80-\x9d\x9f-\xbf]|\xf0\x90\x8f[\x80-\x83\x88-\x95]|\xf0\x90\x92[\x80-\x9d]|\xf0\x90\x94[\x80-\xa7\xb0-\xbf]|\xf0\x90\x95[\x80-\xa3\xaf]|\xf0\x90\x9c[\x80-\xb6]|\xf0\x90\x9d[\x80-\x95\xa0-\xa7]|\xf0\x90\xa0[\x80-\x85\x88\x8a-\xb5\xb7\xb8\xbc\xbf]|\xf0\x90\xa1[\x80-\x95\x97-\xbf]|\xf0\x90\xa2[\x80-\x9e\xa7-\xaf]|\xf0\x90\xa3[\xa0-\xb2\xb4\xb5\xbb-\xbf]|\xf0\x90\xa4[\x80-\x9b\x9f-\xb9\xbf]|\xf0\x90\xa6[\x80-\xb7\xbc-\xbf]|\xf0\x90\xa7[\x80-\x8f\x92-\xbf]|\xf0\x90\xa8[\x80-\x83\x85\x86\x8c-\x93\x95-\x97\x99-\xb3\xb8-\xba\xbf]|\xf0\x90\xa9[\x80-\x87\x90-\x98\xa0-\xbf]|\xf0\x90\xaa[\x80-\x9f]|\xf0\x90\xab[\x80-\xa6\xab-\xb6]|\xf0\x90\xac[\x80-\xb5\xb9-\xbf]|\xf0\x90\xad[\x80-\x95\x98-\xb2\xb8-\xbf]|\xf0\x90\xae[\x80-\x91\x99-\x9c\xa9-\xaf]|\xf0\x90\xb1[\x80-\x88]|\xf0\x90\xb2[\x80-\xb2]|\xf0\x90\xb3[\x80-\xb2\xba-\xbf]|\xf0\x90\xb9[\xa0-\xbe]|\xf0\x91\x80[\x81\x83-\xbf]|\xf0\x91\x81[\x80-\x8d\x92-\xa5\xbf]|\xf0\x91\x82[\x80\x81\x83-\xaf\xb3-\xb6\xb9-\xbc\xbe\xbf]|\xf0\x91\x83[\x80\x81\x90-\xa8]|\xf0\x91\x84[\x80-\xab\xad-\xb4]|\xf0\x91\x85[\x80-\x83\x90-\xb6]|\xf0\x91\x86[\x80\x81\x83-\xb2\xb6-\xbe]|\xf0\x91\x87[\x81-\x8d\x9a-\x9f\xa1-\xb4]|\xf0\x91\x88[\x80-\x91\x93-\xab\xaf-\xb1\xb4\xb6-\xbd]|\xf0\x91\x8a[\x80-\x86\x88\x8a-\x8d\x8f-\x9d\x9f-\xa9\xb0-\xbf]|\xf0\x91\x8b[\x80-\x9f\xa3-\xaa]|\xf0\x91\x8c[\x80\x81\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc\xbd]|\xf0\x91\x8d[\x80\x90\x9d-\xa1\xa6-\xac\xb0-\xb4]|\xf0\x91\x92[\x80-\xaf\xb3-\xb8\xba\xbf]|\xf0\x91\x93[\x80\x82-\x87]|\xf0\x91\x96[\x80-\xae\xb2-\xb5\xbc\xbd\xbf]|\xf0\x91\x97[\x80-\x9d]|\xf0\x91\x98[\x80-\xaf\xb3-\xba\xbd\xbf]|\xf0\x91\x99[\x80-\x84]|\xf0\x91\x9a[\x80-\xab\xad\xb0-\xb5\xb7]|\xf0\x91\x9c[\x80-\x99\x9d-\x9f\xa2-\xa5\xa7-\xab\xba-\xbf]|\xf0\x91\xa2[\xa0-\xbf]|\xf0\x91\xa3[\x80-\x9f\xaa-\xb2\xbf]|\xf0\x91\xab[\x80-\xb8]|\xf0\x92\x8e[\x80-\x99]|\xf0\x92\x91[\x80-\xae\xb0-\xb4]|\xf0\x92\x95[\x80-\x83]|\xf0\x93\x90[\x80-\xae]|\xf0\x94\x99[\x80-\x86]|\xf0\x96\xa8[\x80-\xb8]|\xf0\x96\xa9[\x80-\x9e\xae\xaf]|\xf0\x96\xab[\x90-\xad\xb0-\xb5]|\xf0\x96\xad[\x80-\x85\x9b-\xa1\xa3-\xb7\xbd-\xbf]|\xf0\x96\xae[\x80-\x8f]|\xf0\x96\xbd[\x80-\x84\x90]|\xf0\x96\xbe[\x8f-\x9f]|\xf0\x9b\x80[\x80\x81]|\xf0\x9b\xb1[\x80-\xaa\xb0-\xbc]|\xf0\x9b\xb2[\x80-\x88\x90-\x99\x9c-\x9f]|\xf0\x9d\x83[\x80-\xb5]|\xf0\x9d\x84[\x80-\xa6\xa9-\xbf]|\xf0\x9d\x85[\x80-\xa4\xa7-\xac\xbb-\xbf]|\xf0\x9d\x87[\x80-\xa8]|\xf0\x9d\x89[\x80-\x85]|\xf0\x9d\x8d[\x80-\x96\xa0-\xb1]|\xf0\x9d\x91[\x80-\x94\x96-\xbf]|\xf0\x9d\x92[\x80-\x9c\x9e\x9f\xa2\xa5\xa6\xa9-\xac\xae-\xb9\xbb\xbd-\xbf]|\xf0\x9d\x93[\x80-\x83\x85-\xbf]|\xf0\x9d\x94[\x80-\x85\x87-\x8a\x8d-\x94\x96-\x9c\x9e-\xb9\xbb-\xbe]|\xf0\x9d\x95[\x80-\x84\x86\x8a-\x90\x92-\xbf]|\xf0\x9d\x9a[\x80-\xa5\xa8-\xbf]|\xf0\x9d\x9f[\x80-\x8b]|\xf0\x9d\xaa[\x80-\x8b\x9b-\x9f\xa1-\xaf]|\xf0\x9e\xa3[\x80-\x84\x87-\x96]|\xf0\x9e\xb8[\x80-\x83\x85-\x9f\xa1\xa2\xa4\xa7\xa9-\xb2\xb4-\xb7\xb9\xbb]|\xf0\x9e\xb9[\x82\x87\x89\x8b\x8d-\x8f\x91\x92\x94\x97\x99\x9b\x9d\x9f\xa1\xa2\xa4\xa7-\xaa\xac-\xb2\xb4-\xb7\xb9-\xbc\xbe]|\xf0\x9e\xba[\x80-\x89\x8b-\x9b\xa1-\xa3\xa5-\xa9\xab-\xbb]|\xf0\x9e\xbb[\xb0\xb1]|\xf0\x9f[\x80\x85][\x80-\xab\xb0-\xbf]|\xf0\x9f\x82[\x80-\x93\xa0-\xae\xb1-\xbf]|\xf0\x9f\x83[\x81-\x8f\x91-\xb5]|\xf0\x9f\x84[\x80-\x8c\x90-\xae\xb0-\xbf]|\xf0\x9f\x86[\x80-\x9a]|\xf0\x9f\x87[\xa6-\xbf]|\xf0\x9f\x88[\x80-\x82\x90-\xba]|\xf0\x9f\x89[\x80-\x88\x90\x91]|\xf0\x9f\x95[\x80-\xb9\xbb-\xbf]|\xf0\x9f\x96[\x80-\xa3\xa5-\xbf]|\xf0\x9f\x9b[\x80-\x90\xa0-\xac\xb0-\xb3]|\xf0\x9f\x9d[\x80-\xb3]|\xf0\x9f\x9f[\x80-\x94]|\xf0\x9f\xa0[\x80-\x8b\x90-\xbf]|\xf0\x9f\xa1[\x80-\x87\x90-\x99\xa0-\xbf]|\xf0\x9f\xa2[\x80-\x87\x90-\xad]|\xf0\x9f\xa4[\x90-\x98]|\xf0\x9f\xa6[\x80-\x84]|\xf0\x9f\xa7\x80|\xf0\xaa\x9b[\x80-\x96]|\xf0\xab\x9c[\x80-\xb4]|\xf0\xab\xa0[\x80-\x9d\xa0-\xbf]|\xf0\xac\xba[\x80-\xa1]|\xf0\xaf\xa8[\x80-\x9d]|\xf3[\xb0-\xbe]\xbf[\x80-\xbf]|\xf3[\xb0-\xbf][\x80-\xbe][\x80-\xbf]|\xf3\xa0\x87[\x80-\xaf]|\xf3\xbf\xbf[\x80-\xbd]|\xf4[\x80-\x8e]\xbf[\x80-\xbf]|\xf4[\x80-\x8f][\x80-\xbe][\x80-\xbf]|\xf4\x8f\xbf[\x80-\xbd])|(\xe2\x80\x8c)|(\xe2\x80\x8d))([-\+!$%&\*\/:<=>\?^_~a-zA-Z0-9\.@]|([\xc3-\xcc\xcf-\xd3\xd9\xda][\x80-\xbf]|\xc2[\xa1-\xaa\xac\xae-\xba\xbc-\xbf]|\xcd[\x80-\xb7\xba-\xbf]|\xce[\x84-\x8a\x8c\x8e-\xa1\xa3-\xbf]|\xd4[\x80-\xaf\xb1-\xbf]|\xd5[\x80-\x96\x99-\x9f\xa1-\xbf]|\xd6[\x80-\x87\x89\x8a\x8d-\x8f\x91-\xbf]|\xd7[\x80-\x87\x90-\xaa\xb0-\xb4]|\xd8[\x86-\x9b\x9e-\xbf]|\xdb[\x80-\x9c\x9e-\xbf]|\xdc[\x80-\x8d\x90-\xbf]|\xdd[\x80-\x8a\x8d-\xbf]|\xde[\x80-\xb1]|\xdf[\x80-\xba])|([\xe0\xe2-\xe9\xeb\xec\xee\xef]\xa5[\x80-\xbf]|[\xe0\xe2-\xec\xee\xef]\xa4[\x80-\xbf]|[\xe1-\xe9\xeb-\xef][\x92\x93\x98][\x80-\xbf]|[\xe1-\xe9\xeb\xec\xee\xef]\x9e[\x80-\xbf]|[\xe1-\xe9\xeb\xec\xee]\xac[\x80-\xbf]|[\xe1-\xef][\x85\x94-\x97\x99][\x80-\xbf]|[\xe1\xe2\xe4-\xef][\x84\x87\x88][\x80-\xbf]|[\xe1\xe3-\xec\xee\xef][\xae\xba][\x80-\xbf]|[\xe1\xe3-\xec\xee][\xb4\xb5\xb8\xb9\xbb][\x80-\xbf]|[\xe1\xe3-\xef][\x90\x91][\x80-\xbf]|[\xe1\xe3\xe5-\xec\xee]\xb6[\x80-\xbf]|[\xe1\xe4-\xef][\x80-\x82\x86][\x80-\xbf]|[\xe1\xea]\xa1[\x80-\xb7]|[\xe2-\xe9\xeb-\xef]\x9b[\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee\xef][\xa0\xa1\xa3\xa8][\x80-\xbf]|[\xe2-\xe9\xeb\xec\xee][\xa9\xab][\x80-\xbf]|[\xe2-\xec\xee\xef][\xa2\xaa\xb2][\x80-\xbf]|[\xe2-\xec\xee][\xbc-\xbe][\x80-\xbf]|[\xe2-\xef][\x89\x8a\x8d\x8e\x9a\x9c][\x80-\xbf]|[\xe2\xe4-\xef]\x8b[\x80-\xbf]|[\xe3-\xe8\xea-\xec\xee]\xbf[\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee\xef][\x9f\xa7][\x80-\xbf]|[\xe3-\xe9\xeb\xec\xee][\xad\xaf][\x80-\xbf]|[\xe3-\xec\xee\xef][\xa6\xb0\xb1\xb3][\x80-\xbf]|[\xe3-\xec\xee]\xb7[\x80-\xbf]|[\xe3-\xef][\x83\x8c\x8f\x9d][\x80-\xbf]|\xe0\xa0[\x80-\xad\xb0-\xbe]|\xe0\xa1[\x80-\x9b\x9e]|\xe0\xa2[\xa0-\xb4]|\xe0\xa3[\xa3-\xbf]|\xe0\xa6[\x80-\x83\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb6-\xb9\xbc-\xbf]|\xe0\xa7[\x80-\x84\x87\x88\x8b-\x8e\x97\x9c\x9d\x9f-\xa3\xa6-\xbb]|\xe0\xa8[\x81-\x83\x85-\x8a\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5\xb6\xb8\xb9\xbc\xbe\xbf]|\xe0\xa9[\x80-\x82\x87\x88\x8b-\x8d\x91\x99-\x9c\x9e\xa6-\xb5]|\xe0\xaa[\x81-\x83\x85-\x8d\x8f-\x91\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc-\xbf]|\xe0\xab[\x80-\x85\x87-\x89\x8b-\x8d\x90\xa0-\xa3\xa6-\xb1\xb9]|\xe0\xac[\x81-\x83\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc-\xbf]|\xe0\xad[\x80-\x84\x87\x88\x8b-\x8d\x96\x97\x9c\x9d\x9f-\xa3\xa6-\xb7]|\xe0\xae[\x82\x83\x85-\x8a\x8e-\x90\x92-\x95\x99\x9a\x9c\x9e\x9f\xa3\xa4\xa8-\xaa\xae-\xb9\xbe\xbf]|\xe0\xaf[\x80-\x82\x86-\x88\x8a-\x8d\x90\x97\xa6-\xba]|\xe0\xb0[\x80-\x83\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb9\xbd-\xbf]|\xe0\xb1[\x80-\x84\x86-\x88\x8a-\x8d\x95\x96\x98-\x9a\xa0-\xa3\xa6-\xaf\xb8-\xbf]|\xe0\xb2[\x81-\x83\x85-\x8c\x8e-\x90\x92-\xa8\xaa-\xb3\xb5-\xb9\xbc-\xbf]|\xe0\xb3[\x80-\x84\x86-\x88\x8a-\x8d\x95\x96\x9e\xa0-\xa3\xa6-\xaf\xb1\xb2]|\xe0\xb4[\x81-\x83\x85-\x8c\x8e-\x90\x92-\xba\xbd-\xbf]|\xe0\xb5[\x80-\x84\x86-\x88\x8a-\x8e\x97\x9f-\xa3\xa6-\xb5\xb9-\xbf]|\xe0\xb6[\x82\x83\x85-\x96\x9a-\xb1\xb3-\xbb\xbd]|\xe0\xb7[\x80-\x86\x8a\x8f-\x94\x96\x98-\x9f\xa6-\xaf\xb2-\xb4]|\xe0\xb8[\x81-\xba\xbf]|\xe0\xb9[\x80-\x9b]|\xe0\xba[\x81\x82\x84\x87\x88\x8a\x8d\x94-\x97\x99-\x9f\xa1-\xa3\xa5\xa7\xaa\xab\xad-\xb9\xbb-\xbd]|\xe0\xbb[\x80-\x84\x86\x88-\x8d\x90-\x99\x9c-\x9f]|\xe0\xbc[\x80-\xb9\xbe\xbf]|\xe0\xbd[\x80-\x87\x89-\xac\xb1-\xbf]|\xe0\xbe[\x80-\x97\x99-\xbc\xbe\xbf]|\xe0\xbf[\x80-\x8c\x8e-\x9a]|\xe1\x83[\x80-\x85\x87\x8d\x90-\xbf]|\xe1\x89[\x80-\x88\x8a-\x8d\x90-\x96\x98\x9a-\x9d\xa0-\xbf]|\xe1\x8a[\x80-\x88\x8a-\x8d\x90-\xb0\xb2-\xb5\xb8-\xbe]|\xe1\x8b[\x80\x82-\x85\x88-\x96\x98-\xbf]|\xe1\x8c[\x80-\x90\x92-\x95\x98-\xbf]|\xe1\x8d[\x80-\x9a\x9d-\xbc]|\xe1\x8e[\x80-\x99\xa0-\xbf]|\xe1\x8f[\x80-\xb5\xb8-\xbd]|\xe1\x9a[\x81-\x9a\xa0-\xbf]|\xe1\x9b[\x80-\xb8]|\xe1\x9c[\x80-\x8c\x8e-\x94\xa0-\xb6]|\xe1\x9d[\x80-\x93\xa0-\xac\xae-\xb0\xb2\xb3]|\xe1\x9f[\x80-\x9d\xa0-\xa9\xb0-\xb9]|\xe1\xa0[\x80-\x8d\x90-\x99\xa0-\xbf]|\xe1\xa2[\x80-\xaa\xb0-\xbf]|\xe1\xa3[\x80-\xb5]|\xe1\xa4[\x80-\x9e\xa0-\xab\xb0-\xbb]|\xe1\xa5[\x80\x84-\xad\xb0-\xb4]|\xe1\xa6[\x80-\xab\xb0-\xbf]|\xe1\xa7[\x80-\x89\x90-\x9a\x9e-\xbf]|\xe1\xa8[\x80-\x9b\x9e-\xbf]|\xe1\xa9[\x80-\x9e\xa0-\xbc\xbf]|\xe1\xaa[\x80-\x89\x90-\x99\xa0-\xad\xb0-\xbe]|\xe1\xad[\x80-\x8b\x90-\xbc]|\xe1\xaf[\x80-\xb3\xbc-\xbf]|\xe1\xb0[\x80-\xb7\xbb-\xbf]|\xe1\xb1[\x80-\x89\x8d-\xbf]|\xe1\xb3[\x80-\x87\x90-\xb6\xb8\xb9]|\xe1\xb7[\x80-\xb5\xbc-\xbf]|\xe1\xbc[\x80-\x95\x98-\x9d\xa0-\xbf]|\xe1\xbd[\x80-\x85\x88-\x8d\x90-\x97\x99\x9b\x9d\x9f-\xbd]|\xe1\xbe[\x80-\xb4\xb6-\xbf]|\xe1\xbf[\x80-\x84\x86-\x93\x96-\x9b\x9d-\xaf\xb2-\xb4\xb6-\xbe]|\xe2\x80[\x90-\x97\xa0-\xa7\xb0-\xb8\xbb-\xbf]|\xe2\x81[\x80-\x84\x87-\x9e\xb0\xb1\xb4-\xbc\xbf]|\xe2\x82[\x80-\x8c\x90-\x9c\xa0-\xbe]|\xe2\x83[\x90-\xb0]|\xe2\x86[\x80-\x8b\x90-\xbf]|\xe2\x8c[\x80-\x87\x8c-\xa8\xab-\xbf]|\xe2\x8f[\x80-\xba]|\xe2\x90[\x80-\xa6]|\xe2\x91[\x80-\x8a\xa0-\xbf]|\xe2\x9d[\x80-\xa7\xb6-\xbf]|\xe2\x9f[\x80-\x84\x87-\xa5\xb0-\xbf]|\xe2\xa6[\x80-\x82\x99-\xbf]|\xe2\xa7[\x80-\x97\x9c-\xbb\xbe\xbf]|\xe2\xad[\x80-\xb3\xb6-\xbf]|\xe2\xae[\x80-\x95\x98-\xb9\xbd-\xbf]|\xe2\xaf[\x80-\x88\x8a-\x91\xac-\xaf]|\xe2\xb0[\x80-\xae\xb0-\xbf]|\xe2\xb1[\x80-\x9e\xa0-\xbf]|\xe2\xb3[\x80-\xb3\xb9-\xbf]|\xe2\xb4[\x80-\xa5\xa7\xad\xb0-\xbf]|\xe2\xb5[\x80-\xa7\xaf\xb0\xbf]|\xe2\xb6[\x80-\x96\xa0-\xa6\xa8-\xae\xb0-\xb6\xb8-\xbe]|\xe2\xb7[\x80-\x86\x88-\x8e\x90-\x96\x98-\x9e\xa0-\xbf]|\xe2\xb8[\x80\x81\x86-\x88\x8b\x8e-\x9b\x9e\x9f\xaa-\xbf]|\xe2\xb9[\x80\x81]|\xe2\xba[\x80-\x99\x9b-\xbf]|\xe2\xbb[\x80-\xb3]|\xe2\xbf[\x80-\x95\xb0-\xbb]|\xe3\x80[\x81-\x87\x92\x93\x9c\xa0-\xbf]|\xe3\x81[\x81-\xbf]|\xe3\x82[\x80-\x96\x99-\xbf]|\xe3\x84[\x85-\xad\xb1-\xbf]|\xe3\x86[\x80-\x8e\x90-\xba]|\xe3\x87[\x80-\xa3\xb0-\xbf]|\xe3\x88[\x80-\x9e\xa0-\xbf]|\xe3\x8b[\x80-\xbe]|\xe4\xb6[\x80-\xb5]|\xe9\xbf[\x80-\x95]|\xea\x92[\x80-\x8c\x90-\xbf]|\xea\x93[\x80-\x86\x90-\xbf]|\xea\x98[\x80-\xab]|\xea\x9b[\x80-\xb7]|\xea\x9e[\x80-\xad\xb0-\xb7]|\xea\x9f[\xb7-\xbf]|\xea\xa0[\x80-\xab\xb0-\xb9]|\xea\xa3[\x80-\x84\x8e-\x99\xa0-\xbd]|\xea\xa5[\x80-\x93\x9f-\xbc]|\xea\xa7[\x80-\x8d\x8f-\x99\x9e-\xbe]|\xea\xa8[\x80-\xb6]|\xea\xa9[\x80-\x8d\x90-\x99\x9c-\xbf]|\xea\xab[\x80-\x82\x9b-\xb6]|\xea\xac[\x81-\x86\x89-\x8e\x91-\x96\xa0-\xa6\xa8-\xae\xb0-\xbf]|\xea\xad[\x80-\xa5\xb0-\xbf]|\xea\xaf[\x80-\xad\xb0-\xb9]|\xed\x9e[\x80-\xa3\xb0-\xbf]|\xed\x9f[\x80-\x86\x8b-\xbb]|\xef\xa9[\x80-\xad\xb0-\xbf]|\xef\xab[\x80-\x99]|\xef\xac[\x80-\x86\x93-\x97\x9d-\xb6\xb8-\xbc\xbe]|\xef\xad[\x80\x81\x83\x84\x86-\xbf]|\xef\xaf[\x80\x81\x93-\xbf]|\xef\xb4[\x80-\xbd]|\xef\xb5[\x90-\xbf]|\xef\xb6[\x80-\x8f\x92-\xbf]|\xef\xb7[\x80-\x87\xb0-\xbd]|\xef\xb8[\x80-\x96\x99\xa0-\xb4]|\xef\xb9[\x85\x86\x89-\x92\x94-\x98\x9f-\xa6\xa8-\xab\xb0-\xb4\xb6-\xbf]|\xef\xbb[\x80-\xbc]|\xef\xbc[\x81-\x87\x8a-\xba\xbc\xbe\xbf]|\xef\xbd[\x80-\x9a\x9c\x9e\xa1\xa4-\xbf]|\xef\xbe[\x80-\xbe]|\xef\xbf[\x82-\x87\x8a-\x8f\x92-\x97\x9a-\x9c\xa0-\xa6\xa8-\xae\xbc\xbd])|([\xf0\xf3]\xa0[\x84-\x86][\x80-\xbf]|\xf0[\x90\x91\x94\x9d\x9f-\xac]\x98[\x80-\xbf]|\xf0[\x90\x92\x93\x9d\xa0-\xac]\x82[\x80-\xbf]|\xf0[\x90\x92\x93\xa1-\xac]\x85[\x80-\xbf]|\xf0[\x90\x92\x94\x9d\x9f-\xac]\x90[\x80-\xbf]|\xf0[\x90\x94\x9f-\xac]\x91[\x80-\xbf]|\xf0[\x90\x9b\xa0-\xac]\xb0[\x80-\xbf]|\xf0[\x90\x9d\x9f-\xac]\x99[\x80-\xbf]|\xf0[\x90\x9d\xa0-\xa9\xab\xac]\x9b[\x80-\xbf]|\xf0[\x90\x9f-\xac]\x9a[\x80-\xbf]|\xf0[\x91-\x93\x9d\xa0-\xac]\x80[\x80-\xbf]|\xf0[\x91-\x93\x9d\xa1-\xac]\x86[\x80-\xbf]|\xf0[\x91\x92\x94\x9f-\xac]\x92[\x80-\xbf]|\xf0[\x92\x93\x9d\x9f-\xac][\x81\x8c][\x80-\xbf]|\xf0[\x92\x93\x9d\xa0-\xac]\x88[\x80-\xbf]|\xf0[\x92\x93\x9f-\xac]\x8d[\x80-\xbf]|\xf0[\x92\x93\xa0-\xac][\x83\x87\x89-\x8b][\x80-\xbf]|\xf0[\x92\x93\xa1-\xac]\x84[\x80-\xbf]|\xf0[\x92\x94\x9f-\xac][\x93\x94][\x80-\xbf]|\xf0[\x93\x9f-\xac][\x8e\x8f][\x80-\xbf]|\xf0[\x94\x9d\x9f-\xac]\x97[\x80-\xbf]|\xf0[\x94\x9d\xa0-\xac]\x96[\x80-\xbf]|\xf0[\x94\xa0-\xac]\x95[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xaa\xac\xaf]\xa0[\x80-\xbf]|\xf0[\x96\x9d\x9e\xa0-\xac\xaf][\xa1\xa2][\x80-\xbf]|\xf0[\x96\x9d\xa0-\xac\xaf][\xa3-\xa7][\x80-\xbf]|\xf0[\x96\xa0-\xab]\xbc[\x80-\xbf]|\xf0[\x96\xa0-\xac]\xac[\x80-\xbf]|\xf0[\x9d\x9f-\xaa\xac]\x9c[\x80-\xbf]|\xf0[\x9d\x9f-\xac]\x9e[\x80-\xbf]|\xf0[\x9d\xa0-\xac][\x9d\xa8\xa9][\x80-\xbf]|\xf0[\xa0-\xab][\xba\xbb\xbd-\xbf][\x80-\xbf]|\xf0[\xa0-\xac][\x9f\xaa\xab\xad-\xaf\xb1-\xb9][\x80-\xbf]|\xf0\x90\x80[\x80-\x8b\x8d-\xa6\xa8-\xba\xbc\xbd\xbf]|\xf0\x90\x81[\x80-\x8d\x90-\x9d]|\xf0\x90\x83[\x80-\xba]|\xf0\x90\x84[\x80-\x82\x87-\xb3\xb7-\xbf]|\xf0\x90\x86[\x80-\x8c\x90-\x9b\xa0]|\xf0\x90\x87[\x90-\xbd]|\xf0\x90\x8a[\x80-\x9c\xa0-\xbf]|\xf0\x90\x8b[\x80-\x90\xa0-\xbb]|\xf0\x90\x8c[\x80-\xa3\xb0-\xbf]|\xf0\x90\x8d[\x80-\x8a\x90-\xba]|\xf0\x90\x8e[\x80-\x9d\x9f-\xbf]|\xf0\x90\x8f[\x80-\x83\x88-\x95]|\xf0\x90\x92[\x80-\x9d\xa0-\xa9]|\xf0\x90\x94[\x80-\xa7\xb0-\xbf]|\xf0\x90\x95[\x80-\xa3\xaf]|\xf0\x90\x9c[\x80-\xb6]|\xf0\x90\x9d[\x80-\x95\xa0-\xa7]|\xf0\x90\xa0[\x80-\x85\x88\x8a-\xb5\xb7\xb8\xbc\xbf]|\xf0\x90\xa1[\x80-\x95\x97-\xbf]|\xf0\x90\xa2[\x80-\x9e\xa7-\xaf]|\xf0\x90\xa3[\xa0-\xb2\xb4\xb5\xbb-\xbf]|\xf0\x90\xa4[\x80-\x9b\x9f-\xb9\xbf]|\xf0\x90\xa6[\x80-\xb7\xbc-\xbf]|\xf0\x90\xa7[\x80-\x8f\x92-\xbf]|\xf0\x90\xa8[\x80-\x83\x85\x86\x8c-\x93\x95-\x97\x99-\xb3\xb8-\xba\xbf]|\xf0\x90\xa9[\x80-\x87\x90-\x98\xa0-\xbf]|\xf0\x90\xaa[\x80-\x9f]|\xf0\x90\xab[\x80-\xa6\xab-\xb6]|\xf0\x90\xac[\x80-\xb5\xb9-\xbf]|\xf0\x90\xad[\x80-\x95\x98-\xb2\xb8-\xbf]|\xf0\x90\xae[\x80-\x91\x99-\x9c\xa9-\xaf]|\xf0\x90\xb1[\x80-\x88]|\xf0\x90\xb2[\x80-\xb2]|\xf0\x90\xb3[\x80-\xb2\xba-\xbf]|\xf0\x90\xb9[\xa0-\xbe]|\xf0\x91\x81[\x80-\x8d\x92-\xaf\xbf]|\xf0\x91\x82[\x80-\xbc\xbe\xbf]|\xf0\x91\x83[\x80\x81\x90-\xa8\xb0-\xb9]|\xf0\x91\x84[\x80-\xb4\xb6-\xbf]|\xf0\x91\x85[\x80-\x83\x90-\xb6]|\xf0\x91\x87[\x80-\x8d\x90-\x9f\xa1-\xb4]|\xf0\x91\x88[\x80-\x91\x93-\xbd]|\xf0\x91\x8a[\x80-\x86\x88\x8a-\x8d\x8f-\x9d\x9f-\xa9\xb0-\xbf]|\xf0\x91\x8b[\x80-\xaa\xb0-\xb9]|\xf0\x91\x8c[\x80-\x83\x85-\x8c\x8f\x90\x93-\xa8\xaa-\xb0\xb2\xb3\xb5-\xb9\xbc-\xbf]|\xf0\x91\x8d[\x80-\x84\x87\x88\x8b-\x8d\x90\x97\x9d-\xa3\xa6-\xac\xb0-\xb4]|\xf0\x91\x93[\x80-\x87\x90-\x99]|\xf0\x91\x96[\x80-\xb5\xb8-\xbf]|\xf0\x91\x97[\x80-\x9d]|\xf0\x91\x99[\x80-\x84\x90-\x99]|\xf0\x91\x9a[\x80-\xb7]|\xf0\x91\x9b[\x80-\x89]|\xf0\x91\x9c[\x80-\x99\x9d-\xab\xb0-\xbf]|\xf0\x91\xa2[\xa0-\xbf]|\xf0\x91\xa3[\x80-\xb2\xbf]|\xf0\x91\xab[\x80-\xb8]|\xf0\x92\x8e[\x80-\x99]|\xf0\x92\x91[\x80-\xae\xb0-\xb4]|\xf0\x92\x95[\x80-\x83]|\xf0\x93\x90[\x80-\xae]|\xf0\x94\x99[\x80-\x86]|\xf0\x96\xa8[\x80-\xb8]|\xf0\x96\xa9[\x80-\x9e\xa0-\xa9\xae\xaf]|\xf0\x96\xab[\x90-\xad\xb0-\xb5]|\xf0\x96\xad[\x80-\x85\x90-\x99\x9b-\xa1\xa3-\xb7\xbd-\xbf]|\xf0\x96\xae[\x80-\x8f]|\xf0\x96\xbd[\x80-\x84\x90-\xbe]|\xf0\x96\xbe[\x8f-\x9f]|\xf0\x9b\x80[\x80\x81]|\xf0\x9b\xb1[\x80-\xaa\xb0-\xbc]|\xf0\x9b\xb2[\x80-\x88\x90-\x99\x9c-\x9f]|\xf0\x9d\x83[\x80-\xb5]|\xf0\x9d\x84[\x80-\xa6\xa9-\xbf]|\xf0\x9d\x85[\x80-\xb2\xbb-\xbf]|\xf0\x9d\x87[\x80-\xa8]|\xf0\x9d\x89[\x80-\x85]|\xf0\x9d\x8d[\x80-\x96\xa0-\xb1]|\xf0\x9d\x91[\x80-\x94\x96-\xbf]|\xf0\x9d\x92[\x80-\x9c\x9e\x9f\xa2\xa5\xa6\xa9-\xac\xae-\xb9\xbb\xbd-\xbf]|\xf0\x9d\x93[\x80-\x83\x85-\xbf]|\xf0\x9d\x94[\x80-\x85\x87-\x8a\x8d-\x94\x96-\x9c\x9e-\xb9\xbb-\xbe]|\xf0\x9d\x95[\x80-\x84\x86\x8a-\x90\x92-\xbf]|\xf0\x9d\x9a[\x80-\xa5\xa8-\xbf]|\xf0\x9d\x9f[\x80-\x8b\x8e-\xbf]|\xf0\x9d\xaa[\x80-\x8b\x9b-\x9f\xa1-\xaf]|\xf0\x9e\xa3[\x80-\x84\x87-\x96]|\xf0\x9e\xb8[\x80-\x83\x85-\x9f\xa1\xa2\xa4\xa7\xa9-\xb2\xb4-\xb7\xb9\xbb]|\xf0\x9e\xb9[\x82\x87\x89\x8b\x8d-\x8f\x91\x92\x94\x97\x99\x9b\x9d\x9f\xa1\xa2\xa4\xa7-\xaa\xac-\xb2\xb4-\xb7\xb9-\xbc\xbe]|\xf0\x9e\xba[\x80-\x89\x8b-\x9b\xa1-\xa3\xa5-\xa9\xab-\xbb]|\xf0\x9e\xbb[\xb0\xb1]|\xf0\x9f[\x80\x85][\x80-\xab\xb0-\xbf]|\xf0\x9f\x82[\x80-\x93\xa0-\xae\xb1-\xbf]|\xf0\x9f\x83[\x81-\x8f\x91-\xb5]|\xf0\x9f\x84[\x80-\x8c\x90-\xae\xb0-\xbf]|\xf0\x9f\x86[\x80-\x9a]|\xf0\x9f\x87[\xa6-\xbf]|\xf0\x9f\x88[\x80-\x82\x90-\xba]|\xf0\x9f\x89[\x80-\x88\x90\x91]|\xf0\x9f\x95[\x80-\xb9\xbb-\xbf]|\xf0\x9f\x96[\x80-\xa3\xa5-\xbf]|\xf0\x9f\x9b[\x80-\x90\xa0-\xac\xb0-\xb3]|\xf0\x9f\x9d[\x80-\xb3]|\xf0\x9f\x9f[\x80-\x94]|\xf0\x9f\xa0[\x80-\x8b\x90-\xbf]|\xf0\x9f\xa1[\x80-\x87\x90-\x99\xa0-\xbf]|\xf0\x9f\xa2[\x80-\x87\x90-\xad]|\xf0\x9f\xa4[\x90-\x98]|\xf0\x9f\xa6[\x80-\x84]|\xf0\x9f\xa7\x80|\xf0\xaa\x9b[\x80-\x96]|\xf0\xab\x9c[\x80-\xb4]|\xf0\xab\xa0[\x80-\x9d\xa0-\xbf]|\xf0\xac\xba[\x80-\xa1]|\xf0\xaf\xa8[\x80-\x9d]|\xf3[\xb0-\xbe]\xbf[\x80-\xbf]|\xf3[\xb0-\xbf][\x80-\xbe][\x80-\xbf]|\xf3\xa0\x87[\x80-\xaf]|\xf3\xbf\xbf[\x80-\xbd]|\xf4[\x80-\x8e]\xbf[\x80-\xbf]|\xf4[\x80-\x8f][\x80-\xbe][\x80-\xbf]|\xf4\x8f\xbf[\x80-\xbd])|(\xe2\x80\x8c)|(\xe2\x80\x8d))*\| {
  yytext[yyleng - 1] = '\0';
  yytext++;
  yylval.obj = identifier_new(yytext);
  return TOKEN_IDENTIFIER;
}
 /* | ⟨symbol element⟩* | */
"||" {
  char *str = strdup("");
  yylval.obj = identifier_vertical_new(str);
  free(str);
  return TOKEN_IDENTIFIER_VERTICAL;
}

"|" {
  BEGIN IDENTIFIER_VERTICAL_LEX;
  sym_list = NULL;
}
<IDENTIFIER_VERTICAL_LEX>"|" {
  BEGIN INITIAL;
  sym_list = g_slist_reverse(sym_list);
  guint len = g_slist_length(sym_list);
  identifier_vertical_str = malloc(sizeof(char) * (len + 1) );
  g_slist_foreach(sym_list, identifier_vertical_func, NULL);
  identifier_vertical_str[identifier_vertical_i] = '\0';
  identifier_vertical_i = 0;
  yylval.obj = identifier_vertical_new(identifier_vertical_str);
  free(identifier_vertical_str);
  g_slist_free(sym_list);
  return TOKEN_IDENTIFIER_VERTICAL;
}
 /* inline hex escape */
<IDENTIFIER_VERTICAL_LEX>\\[xX][0-9a-fA-F]+";" {
  yytext[yyleng - 1] = '\0';
  yytext += 2;  
  char *endptr = NULL;
  gunichar c = (gunichar)strtol(yytext, &endptr, 16);
  char outbuf[6];
  int len = g_unichar_to_utf8(c, outbuf);
  for (size_t i = 0; i < len; i++) {
    sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER(outbuf[i]));
  }
}
<IDENTIFIER_VERTICAL_LEX>\\"|" {
  sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER('|'));
}
 /* mnemonic escape */
<IDENTIFIER_VERTICAL_LEX>\\a {
  sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER('\a'));
}
<IDENTIFIER_VERTICAL_LEX>\\b {
  sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER('\b'));
}
<IDENTIFIER_VERTICAL_LEX>\\t {
  sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER('\t'));
}
<IDENTIFIER_VERTICAL_LEX>\\n {
  sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER('\n'));
}
<IDENTIFIER_VERTICAL_LEX>\\r {
  sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER('\r'));
}
<IDENTIFIER_VERTICAL_LEX>[^\\|] {
   for (size_t i = 0; yytext[i] != '\0'; i++) {
     sym_list = g_slist_prepend(sym_list, GUINT_TO_POINTER(yytext[i]));
   }
}

 /* explicit sign */
[+-] {
  yylval.obj = identifier_new(yytext);
  return TOKEN_IDENTIFIER;
}

 /* boolean */
#[tT] |
#[tT][rR][uU][eE] { yylval.obj = true_obj; return TOKEN_BOOLEAN;}
#[fF] |
#[fF][aA][lL][sS][eE] {yylval.obj = false_obj; return TOKEN_BOOLEAN;}

 /* number */
 /* radix 10 */
#[dD][-+]?[0-9]+ |
#[eE][-+]?[0-9]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 10);
  return TOKEN_NUMBERZ;
}
#[eE][-+]?[0-9]+(e|E)[-+]?[0-9]+ {
  yylval.obj.type = NUMBERQ;
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpf_set_str(opf, yytext, 10);
  mpq_init(yylval.obj.numberq);
  mpq_set_f(yylval.obj.numberq, opf);
  return TOKEN_NUMBERQ;
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 10);
  return TOKEN_NUMBERZ;
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+(e|E)[-+]?[0-9]+ {
  yylval.obj.type = NUMBERQ;
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpf_set_str(opf, yytext, 10);
  mpq_init(yylval.obj.numberq);
  mpq_set_f(yylval.obj.numberq, opf);
  return TOKEN_NUMBERQ;
}
[-+]?[0-9]+ {
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 10);
  return TOKEN_NUMBERZ;
}
#[dD][-+]?[0-9]+"/"[0-9]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 10);
  return TOKEN_NUMBERQ;
}
#[eE][-+]?"."[0-9]+ |
#[eE][-+]?"."[0-9]+(e|E)[-+]?[0-9]+ |
#[eE][-+]?[0-9]+"."[0-9]* |
#[eE][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+ {
  yylval.obj.type = NUMBERQ;
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpf_set_str(opf, yytext, 10);
  mpq_init(yylval.obj.numberq);
  mpq_set_f(yylval.obj.numberq, opf);
  return TOKEN_NUMBERQ;
}
#[eE][-+]?[0-9]+"/"[0-9]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 10);
  return TOKEN_NUMBERQ;
}
#[eE][-+][iI][nN][fF]"."0 {
  int sign = yytext[2] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
#[eE][-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?"."[0-9]+ |
(#[dD]#[eE]|#[eE]#[dD])[-+]?"."[0-9]+(e|E)[-+]?[0-9]+ |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"."[0-9]* |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+ {
  yylval.obj.type = NUMBERQ;
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpf_set_str(opf, yytext, 10);
  mpq_init(yylval.obj.numberq);
  mpq_set_f(yylval.obj.numberq, opf);
  return TOKEN_NUMBERQ;
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"/"[0-9]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 10);
  return TOKEN_NUMBERQ;
}
(#[dD]#[eE]|#[eE]#[dD])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[dD]#[eE]|#[eE]#[dD])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
[-+]?[0-9]+"/"[0-9]+ {
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 10);
  return TOKEN_NUMBERQ;
}
#[dD][-+]?"."[0-9]+ |
#[dD][-+]?"."[0-9]+(e|E)[-+]?[0-9]+ |
#[dD][-+]?[0-9]+"."[0-9]* |
#[dD][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+ |
#[dD][-+]?[0-9]+(e|E)[-+]?[0-9]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberr_new(yytext, 10);
  return TOKEN_NUMBERR;
}
#[dD][-+][iI][nN][fF]"."0 {
  int sign = yytext[2] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
#[dD][-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
#[iI][-+]?"."[0-9]+ |
#[iI][-+]?"."[0-9]+(e|E)[-+]?[0-9]+ |
#[iI][-+]?[0-9]+ |
#[iI][-+]?[0-9]+"."[0-9]* |
#[iI][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+ |
#[iI][-+]?[0-9]+"/"[0-9]+ |
#[iI][-+]?[0-9]+(e|E)[-+]?[0-9]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberr_new(yytext, 10);
  return TOKEN_NUMBERR;
}
#[iI][-+][iI][nN][fF]"."0 {
  int sign = yytext[2] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
#[iI][-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
(#[dD]#[iI]|#[iI]#[dD])[-+]?"."[0-9]+ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?"."[0-9]+(e|E)[-+]?[0-9]+ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"."[0-9]* |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+(e|E)[-+]?[0-9]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberr_new(yytext, 10);
  return TOKEN_NUMBERR;
}
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"/"[0-9]+ {
  yylval.obj.type = NUMBERR;
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_init_set_q(yylval.obj.numberr, opq, MPFR_RNDN);
  return TOKEN_NUMBERR;  
}
(#[dD]#[iI]|#[iI]#[dD])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[dD]#[iI]|#[iI]#[dD])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
[-+]?"."[0-9]+ |
[-+]?"."[0-9]+(e|E)[-+]?[0-9]+ |
[-+]?[0-9]+"."[0-9]* |
[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+ |
[-+]?[0-9]+(e|E)[-+]?[0-9]+ {
  if (*yytext == '+') yytext++;
  yylval.obj = numberr_new(yytext, 10);
  return TOKEN_NUMBERR;
}
[-+][iI][nN][fF]"."0 {
  int sign = yytext[0] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
[-+][nN][aA][nN]"."0 {  
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
#[dD][-+]?[0-9]+[-+] |
#[eE][-+]?[0-9]+[-+] |
#[eE][-+]?[0-9]+(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+[-+] |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
#[dD][-+]?[0-9]+"/"[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[eE][-+]?"."[0-9]+[-+] |
#[eE][-+]?"."[0-9]+(e|E)[-+]?[0-9]+[-+] |
#[eE][-+]?[0-9]+"."[0-9]*[-+] |
#[eE][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
#[eE][-+]?[0-9]+"/"[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[eE][-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[eE][-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  mpfr_set_nan(real);
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?"."[0-9]+[-+] |
(#[dD]#[eE]|#[eE]#[dD])[-+]?"."[0-9]+(e|E)[-+]?[0-9]+[-+] |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"."[0-9]*[-+] |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"/"[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
(#[dD]#[eE]|#[eE]#[dD])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[dD]#[eE]|#[eE]#[dD])[-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  mpfr_set_nan(real);
}
[-+]?[0-9]+"/"[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[dD][-+]?"."[0-9]+[-+] |
#[dD][-+]?"."[0-9]+(e|E)[-+]?[0-9]+[-+] |
#[dD][-+]?[0-9]+"."[0-9]*[-+] |
#[dD][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[-+] |
#[dD][-+]?[0-9]+(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
#[dD][-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[2] == '+' ? 1 : -1;
  
  mpfr_set_inf(real, sign);
}
#[dD][-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  
  mpfr_set_nan(real);
}
#[iI][-+]?"."[0-9]+[-+] |
#[iI][-+]?"."[0-9]+(e|E)[-+]?[0-9]+[-+] |
#[iI][-+]?[0-9]+[-+] |
#[iI][-+]?[0-9]+"."[0-9]*[-+] |
#[iI][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[-+] |
#[iI][-+]?[0-9]+(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
#[iI][-+]?[0-9]+"/"[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[iI][-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[2] == '+' ? 1 : -1;
  
  mpfr_set_inf(real, sign);
}
#[iI][-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  
  mpfr_set_nan(real);
}

(#[dD]#[iI]|#[iI]#[dD])[-+]?"."[0-9]+[-+] |
(#[dD]#[iI]|#[iI]#[dD])[-+]?"."[0-9]+(e|E)[-+]?[0-9]+[-+] |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+[-+] |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"."[0-9]*[-+] |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[-+] |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"/"[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
(#[dD]#[iI]|#[iI]#[dD])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  
  mpfr_set_inf(real, sign);
}
(#[dD]#[iI]|#[iI]#[dD])[-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  
  mpfr_set_nan(real);
}
[-+]?"."[0-9]+[-+] |
[-+]?"."[0-9]+(e|E)[-+]?[0-9]+[-+] |
[-+]?[0-9]+"."[0-9]*[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[-+] |
[-+]?[0-9]+(e|E)[-+]?[0-9]+[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[0] == '+' ? 1 : -1;
  
  mpfr_set_inf(real, sign);
}
[-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG;
  sign_imag = yytext[yyleng - 1];
  mpfr_set_nan(real);
}
<IMAG>[0-9]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  mpfr_set_str(opfr, yytext, 10, MPFR_RNDN);
  yylval.obj = numberc_new_r_istr(real, yytext, 10);
  if (sign_imag == '-') {
    mpfr_neg(mpc_imagref(yylval.obj.numberc),mpc_imagref(yylval.obj.numberc),
                          MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
<IMAG>[0-9]+"/"[0-9]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(opfr, opq, MPFR_RNDN);
  if (sign_imag == '-') {
    mpfr_neg(opfr, opfr, MPFR_RNDN);
  }
  mpc_set_fr_fr(yylval.obj.numberc, real, opfr, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG>[nN][aA][nN]"."0[iI] {
  BEGIN INITIAL;
  mpfr_t imag;
  mpfr_set_nan(imag);
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG>[0-9]*"."[0-9]+(e|E)[-+]?[0-9]+[iI] |
<IMAG>[0-9]*"."[0-9]+[iI] |
<IMAG>[0-9]+(e|E)[-+]?[0-9]+[iI] {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);  
  yytext[yyleng - 1] = '\0';  
  mpfr_set_str(imag, yytext, 10, MPFR_RNDN);
  if (sign_imag == '-') {
    mpfr_neg(imag, imag, MPFR_RNDN);
  }
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG>[iI] {
  BEGIN INITIAL;
  if (sign_imag == '+') {    
    yylval.obj = numberc_new_r_istr(real, "1", 10);
  } else {
    yylval.obj = numberc_new_r_istr(real, "-1", 10);
  }  
  return TOKEN_NUMBERC;
}
<IMAG>[iI][nN][fF]".0"[iI] {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;  
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set(mpc_realref(yylval.obj.numberc), real, MPFR_RNDN);
  int sign = sign_imag == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;  
}
 /* 0+imag */
[-+][0-9]+(e|E)[-+]?[0-9]+[iI] |
[-+]"."[0-9]+(e|E)[-+]?[0-9]+[iI] |
[-+][0-9]+"."[0-9]*[iI] |
[-+][0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 10, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
[-+][iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  if (*yytext == '+') { 
    mpfr_set_str(mpc_imagref(yylval.obj.numberc), "1", 10, MPFR_RNDN);
  } else {
    mpfr_set_str(mpc_imagref(yylval.obj.numberc), "-1", 10, MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
[-+][0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 10, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
[-+][0-9]+"/"[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
[-+]"."[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 10, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[dD][-+][0-9]+[iI] |
#[eE][-+][0-9]+[iI] |
#[eE][-+][0-9]+(e|E)[-+]?[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 10, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[dD][-+][0-9]+"/"[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[eE][-+]"."[0-9]+[iI] |
#[eE][-+]"."[0-9]+(e|E)[-+]?[0-9]+[iI] |
#[eE][-+][0-9]+"."[0-9]*[iI] |
#[eE][-+][0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 10, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[eE][-+][0-9]+"/"[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[eE][-+][iI][nN][fF]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_si(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
#[eE][-+][nN][aA][nN]"."0[iI]  {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}
(#[dD]#[eE]|#[eE]#[dD])[-+][0-9]+[iI] |
(#[dD]#[eE]|#[eE]#[dD])[-+][0-9]+(e|E)[-+]?[0-9]+[iI] |
(#[dD]#[eE]|#[eE]#[dD])[-+]"."[0-9]+[iI] |
(#[dD]#[eE]|#[eE]#[dD])[-+]"."[0-9]+(e|E)[-+]?[0-9]+[iI] |
(#[dD]#[eE]|#[eE]#[dD])[-+][0-9]+"."[0-9]*[iI] |
(#[dD]#[eE]|#[eE]#[dD])[-+][0-9]+"."[0-9]*(e|E)[-+]?[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 10, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[dD]#[eE]|#[eE]#[dD])[-+][0-9]+"/"[0-9]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[dD]#[eE]|#[eE]#[dD])[-+][iI][nN][fF]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_si(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
(#[dD]#[eE]|#[eE]#[dD])[-+][nN][aA][nN]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}

 /* polar real@arg */
#[dD][-+]?[0-9]+@ |
#[eE][-+]?[0-9]+@ |
#[eE][-+]?[0-9]+(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+@ |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
#[dD][-+]?[0-9]+"/"[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[eE][-+]?"."[0-9]+@ |
#[eE][-+]?"."[0-9]+(e|E)[-+]?[0-9]+@ |
#[eE][-+]?[0-9]+"."[0-9]*@ |
#[eE][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
#[eE][-+]?[0-9]+"/"[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[eE][-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_POLAR;
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[eE][-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_POLAR;
  mpfr_set_nan(real);
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?"."[0-9]+@ |
(#[dD]#[eE]|#[eE]#[dD])[-+]?"."[0-9]+(e|E)[-+]?[0-9]+@ |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"."[0-9]*@ |
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
(#[dD]#[eE]|#[eE]#[dD])[-+]?[0-9]+"/"[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
(#[dD]#[eE]|#[eE]#[dD])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[dD]#[eE]|#[eE]#[dD])[-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_POLAR;
  mpfr_set_nan(real);
}
[-+]?[0-9]+"/"[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[dD][-+]?"."[0-9]+@ |
#[dD][-+]?"."[0-9]+(e|E)[-+]?[0-9]+@ |
#[dD][-+]?[0-9]+"."[0-9]*@ |
#[dD][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+@ |
#[dD][-+]?[0-9]+(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
#[dD][-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_POLAR;
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[dD][-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_POLAR;
  mpfr_set_nan(real);
}
#[iI][-+]?"."[0-9]+@ |
#[iI][-+]?"."[0-9]+(e|E)[-+]?[0-9]+@ |
#[iI][-+]?[0-9]+@ |
#[iI][-+]?[0-9]+"."[0-9]*@ |
#[iI][-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+@ |
#[iI][-+]?[0-9]+(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
#[iI][-+]?[0-9]+"/"[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
#[iI][-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_POLAR;
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[iI][-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_POLAR;
  mpfr_set_nan(real);
}

(#[dD]#[iI]|#[iI]#[dD])[-+]?"."[0-9]+@ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?"."[0-9]+(e|E)[-+]?[0-9]+@ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+@ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"."[0-9]*@ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+@ |
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);  
}
(#[dD]#[iI]|#[iI]#[dD])[-+]?[0-9]+"/"[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(real, opq, MPFR_RNDN);
}
(#[dD]#[iI]|#[iI]#[dD])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[dD]#[iI]|#[iI]#[dD])[-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_POLAR;
  mpfr_set_nan(real);
}
[-+]?"."[0-9]+@ |
[-+]?"."[0-9]+(e|E)[-+]?[0-9]+@ |
[-+]?[0-9]+"."[0-9]*@ |
[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+@ |
[-+]?[0-9]+(e|E)[-+]?[0-9]+@ {
  BEGIN IMAG_POLAR;
  yytext[yyleng - 1] = '\0';
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 10, MPFR_RNDN);
}
[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_POLAR;
  int sign = yytext[0] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
[-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_POLAR;
  mpfr_set_nan(real);
}
<IMAG_POLAR>[-+]?"."[0-9]+ |
<IMAG_POLAR>[-+]?"."[0-9]+(e|E)[-+]?[0-9]+ |
<IMAG_POLAR>[-+]?[0-9]+ |
<IMAG_POLAR>[-+]?[0-9]+"."[0-9]* |
<IMAG_POLAR>[-+]?[0-9]+"."[0-9]*(e|E)[-+]?[0-9]+ |
<IMAG_POLAR>[-+]?[0-9]+(e|E)[-+]?[0-9]+ {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(argopfr, yytext, 10, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_POLAR>[-+]?[0-9]+"/"[0-9]+ {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 10);
  mpfr_set_q(argopfr, opq, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_POLAR>[-+][iI][nN][fF]"."0 {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  int sign = yytext[0] == '+' ? 1 : -1;
  mpfr_set_inf(argopfr, sign);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}
<IMAG_POLAR>[-+][nN][aA][nN]"."0 {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_nan(argopfr);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}

 /* number radix 2 */
#[bB][-+]?[01]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 2);
  return TOKEN_NUMBERZ;
}
(#[bB]#[eE]|#[eE]#[bB])[-+]?[01]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 2);
  return TOKEN_NUMBERZ;
}
#[bB][-+]?[01]+"/"[01]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 2);
  return TOKEN_NUMBERQ;
}
(#[bB]#[eE]|#[eE]#[bB])[-+]?[01]+"/"[01]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 2);
  return TOKEN_NUMBERQ;
}
(#[bB]#[eE]|#[eE]#[bB])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[bB]#[eE]|#[eE]#[bB])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
#[bB][-+][iI][nN][fF]"."0 {
  int sign = yytext[2] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
#[bB][-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
(#[bB]#[iI]|#[iI]#[bB])[-+]?[01]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberr_new(yytext, 2);
  return TOKEN_NUMBERR;
}
(#[bB]#[iI]|#[iI]#[bB])[-+]?[01]+"/"[01]+ {
  yylval.obj.type = NUMBERR;
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 2);
  mpfr_init_set_q(yylval.obj.numberr, opq, MPFR_RNDN);
  return TOKEN_NUMBERR;
}
(#[bB]#[iI]|#[iI]#[bB])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[bB]#[iI]|#[iI]#[bB])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
 /* real+imag */
#[bB][-+]?[01]+[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 2, MPFR_RNDN);    
}
(#[bB]#[eE]|#[eE]#[bB])[-+]?[01]+[-+] |
(#[bB]#[iI]|#[iI]#[bB])[-+]?[01]+[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 2, MPFR_RNDN);    
}
#[bB][-+]?[01]+"/"[01]+[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[bB]#[eE]|#[eE]#[bB])[-+]?[01]+"/"[01]+[-+] |
(#[bB]#[iI]|#[iI]#[bB])[-+]?[01]+"/"[01]+[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[bB]#[eE]|#[eE]#[bB])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[bB][-+][nN][aA][nN]"."0[-+] |
(#[bB]#[eE]|#[eE]#[bB])[-+][nN][aA][nN]"."0[-+] |
(#[bB]#[iI]|#[iI]#[bB])[-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];
  mpfr_set_nan(real);
}
#[bB][-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[bB]#[iI]|#[iI]#[bB])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_BIN;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}

<IMAG_BIN>[01]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  yylval.obj = numberc_new_r_istr(real, yytext, 2);
  if (sign_imag == '-') {
    mpfr_neg(mpc_imagref(yylval.obj.numberc),mpc_imagref(yylval.obj.numberc),
                          MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
<IMAG_BIN>[01]+"/"[01]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(opfr, opq, MPFR_RNDN);
  if (sign_imag == '-') {
    mpfr_neg(opfr, opfr, MPFR_RNDN);
  }
  mpc_set_fr_fr(yylval.obj.numberc, real, opfr, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_BIN>[iI] {
  BEGIN INITIAL;
  if (sign_imag == '+') {    
    yylval.obj = numberc_new_r_istr(real, "1", 2);
  } else {
    yylval.obj = numberc_new_r_istr(real, "-1", 2);
  }  
  return TOKEN_NUMBERC;
}
<IMAG_BIN>[-+][iI][nN][fF]"."0[iI] {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;  
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set(mpc_realref(yylval.obj.numberc), real, MPFR_RNDN);
  int sign = sign_imag == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC; 
}
<IMAG_BIN>[nN][aA][nN]"."0[iI] {
  BEGIN INITIAL;
  mpfr_t imag;
  mpfr_set_nan(imag);
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
 /* #b0+imag */
#[bB][-+][iI] {  
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  if (yytext[2] == '+') {
    mpfr_set_si(mpc_imagref(yylval.obj.numberc), 1, MPFR_RNDN);
  } else {
    mpfr_set_si(mpc_imagref(yylval.obj.numberc), -1, MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
#[bB][-+][01]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 2, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[bB]#[eE]|#[eE]#[bB])[-+][01]+[iI] |
(#[bB]#[iI]|#[iI]#[bB])[-+][01]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 2, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[bB][-+][01]+"/"[01]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[bB]#[eE]|#[eE]#[bB])[-+][01]+"/"[01]+[iI] |
(#[bB]#[iI]|#[iI]#[bB])[-+][01]+"/"[01]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[bB][-+][iI][nN][fF]"."0[iI]  {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);  
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign =  yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
#[bB][-+][nN][aA][nN]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}
(#[bB]#[iI]|#[iI]#[bB])[-+][iI][nN][fF]"."0[iI] |
(#[bB]#[eE]|#[eE]#[bB])[-+][iI][nN][fF]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);  
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign =  yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
(#[bB]#[eE]|#[eE]#[bB])[-+][nN][aA][nN]"."0[iI] |
(#[bB]#[iI]|#[iI]#[bB])[-+][nN][aA][nN]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}

 /* polar radix 2 */
#[bB][-+]?[01]+@ {
  BEGIN IMAG_BIN_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 2, MPFR_RNDN);    
}
(#[bB]#[eE]|#[eE]#[bB])[-+]?[01]+@ |
(#[bB]#[iI]|#[iI]#[bB])[-+]?[01]+@ {
  BEGIN IMAG_BIN_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 2, MPFR_RNDN);    
}
#[bB][-+]?[01]+"/"[01]+@ {
  BEGIN IMAG_BIN_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[bB]#[eE]|#[eE]#[bB])[-+]?[01]+"/"[01]+@ |
(#[bB]#[iI]|#[iI]#[bB])[-+]?[01]+"/"[01]+@ {
  BEGIN IMAG_BIN_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[bB]#[eE]|#[eE]#[bB])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_BIN_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[bB][-+][nN][aA][nN]"."0@ |
(#[bB]#[eE]|#[eE]#[bB])[-+][nN][aA][nN]"."0@ |
(#[bB]#[iI]|#[iI]#[bB])[-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_BIN_POLAR;
  mpfr_set_nan(real);
}
#[bB][-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_BIN_POLAR;
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[bB]#[iI]|#[iI]#[bB])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_BIN_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}

<IMAG_BIN_POLAR>[-+]?[01]+  {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(argopfr, yytext, 2, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_BIN_POLAR>[-+]?[01]+"/"[01]+ {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 2);
  mpfr_set_q(argopfr, opq, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_BIN_POLAR>[-+][iI][nN][fF]"."0 {
 BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  int sign = yytext[0] == '+' ? 1 : -1;
  mpfr_set_inf(argopfr, sign);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}
<IMAG_BIN_POLAR>[-+][nN][aA][nN]"."0  {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_nan(argopfr);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}
 /* number radix 8  */
#[oO][-+]?[0-8]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 8);
  return TOKEN_NUMBERZ;
}
(#[oO]#[eE]|#[eE]#[oO])[-+]?[0-8]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 8);
  return TOKEN_NUMBERZ;
}
#[oO][-+]?[0-8]+"/"[0-8]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 8);
  return TOKEN_NUMBERQ;
}
(#[oO]#[eE]|#[eE]#[oO])[-+]?[0-8]+"/"[0-8]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 8);
  return TOKEN_NUMBERQ;
}
(#[oO]#[eE]|#[eE]#[oO])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[oO]#[eE]|#[eE]#[oO])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
#[oO][-+][iI][nN][fF]"."0 {
  int sign = yytext[2] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
#[oO][-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
(#[oO]#[iI]|#[iI]#[oO])[-+]?[0-8]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberr_new(yytext, 8);
  return TOKEN_NUMBERR;
}
(#[oO]#[iI]|#[iI]#[oO])[-+]?[0-8]+"/"[0-8]+ {
  yylval.obj.type = NUMBERR;
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 8);
  mpfr_init_set_q(yylval.obj.numberr, opq, MPFR_RNDN);
  return TOKEN_NUMBERR;
}
(#[oO]#[iI]|#[iI]#[oO])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[oO]#[iI]|#[iI]#[oO])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
 /* real+imag */
#[oO][-+]?[0-8]+[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 8, MPFR_RNDN);    
}
(#[oO]#[eE]|#[eE]#[oO])[-+]?[0-8]+[-+] |
(#[oO]#[iI]|#[iI]#[oO])[-+]?[0-8]+[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 2, MPFR_RNDN);    
}
#[oO][-+]?[0-8]+"/"[0-8]+[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[oO]#[eE]|#[eE]#[oO])[-+]?[0-8]+"/"[0-8]+[-+] |
(#[oO]#[iI]|#[iI]#[oO])[-+]?[0-8]+"/"[0-8]+[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[oO]#[eE]|#[eE]#[oO])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[oO][-+][nN][aA][nN]"."0[-+] |
(#[oO]#[eE]|#[eE]#[oO])[-+][nN][aA][nN]"."0[-+] |
(#[oO]#[iI]|#[iI]#[oO])[-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];
  mpfr_set_nan(real);
}
#[oO][-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[oO]#[iI]|#[iI]#[oO])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_OCT;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}

<IMAG_OCT>[0-7]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  yylval.obj = numberc_new_r_istr(real, yytext, 8);
  if (sign_imag == '-') {
    mpfr_neg(mpc_imagref(yylval.obj.numberc),mpc_imagref(yylval.obj.numberc),
                          MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
<IMAG_OCT>[0-7]+"/"[0-7]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(opfr, opq, MPFR_RNDN);
  if (sign_imag == '-') {
    mpfr_neg(opfr, opfr, MPFR_RNDN);
  }
  mpc_set_fr_fr(yylval.obj.numberc, real, opfr, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_OCT>[iI] {
  BEGIN INITIAL;
  if (sign_imag == '+') {    
    yylval.obj = numberc_new_r_istr(real, "1", 8);
  } else {
    yylval.obj = numberc_new_r_istr(real, "-1", 8);
  }  
  return TOKEN_NUMBERC;
}
<IMAG_OCT>[-+][iI][nN][fF]"."0[iI] {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;  
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set(mpc_realref(yylval.obj.numberc), real, MPFR_RNDN);
  int sign = sign_imag == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC; 
}
<IMAG_OCT>[nN][aA][nN]"."0[iI] {
  BEGIN INITIAL;
  mpfr_t imag;
  mpfr_set_nan(imag);
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
 /* #o0+imag */
#[oO][-+][iI] {  
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  if (yytext[2] == '+') {
    mpfr_set_si(mpc_imagref(yylval.obj.numberc), 1, MPFR_RNDN);
  } else {
    mpfr_set_si(mpc_imagref(yylval.obj.numberc), -1, MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
#[oO][-+][0-8]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 2, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[oO]#[eE]|#[eE]#[oO])[-+][0-8]+[iI] |
(#[oO]#[iI]|#[iI]#[oO])[-+][0-8]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 2, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[oO][-+][0-8]+"/"[0-8]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[oO]#[eE]|#[eE]#[oO])[-+][0-8]+"/"[0-8]+[iI] |
(#[oO]#[iI]|#[iI]#[oO])[-+][0-8]+"/"[0-8]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[oO][-+][iI][nN][fF]"."0[iI]  {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);  
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign =  yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
#[oO][-+][nN][aA][nN]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}
(#[oO]#[iI]|#[iI]#[oO])[-+][iI][nN][fF]"."0[iI] |
(#[oO]#[eE]|#[eE]#[oO])[-+][iI][nN][fF]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);  
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign =  yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
(#[oO]#[eE]|#[eE]#[oO])[-+][nN][aA][nN]"."0[iI] |
(#[oO]#[iI]|#[iI]#[oO])[-+][nN][aA][nN]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}

 /* polar  */
#[oO][-+]?[0-8]+@ {
  BEGIN IMAG_OCT_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 8, MPFR_RNDN);    
}
(#[oO]#[eE]|#[eE]#[oO])[-+]?[0-8]+@ |
(#[oO]#[iI]|#[iI]#[oO])[-+]?[0-8]+@ {
  BEGIN IMAG_OCT_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 8, MPFR_RNDN);    
}
#[oO][-+]?[0-8]+"/"[0-8]+@ {
  BEGIN IMAG_OCT_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[oO]#[eE]|#[eE]#[oO])[-+]?[0-8]+"/"[0-8]+@ |
(#[oO]#[iI]|#[iI]#[oO])[-+]?[0-8]+"/"[0-8]+@ {
  BEGIN IMAG_OCT_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[oO]#[eE]|#[eE]#[oO])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_OCT_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[oO][-+][nN][aA][nN]"."0@ |
(#[oO]#[eE]|#[eE]#[oO])[-+][nN][aA][nN]"."0@ |
(#[oO]#[iI]|#[iI]#[oO])[-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_OCT_POLAR;
  mpfr_set_nan(real);
}
#[oO][-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_OCT_POLAR;
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[oO]#[iI]|#[iI]#[oO])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_OCT_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}

<IMAG_OCT_POLAR>[-+]?[0-8]+  {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(argopfr, yytext, 8, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_OCT_POLAR>[-+]?[0-8]+"/"[0-8]+ {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 8);
  mpfr_set_q(argopfr, opq, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_OCT_POLAR>[-+][iI][nN][fF]"."0 {
 BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  int sign = yytext[0] == '+' ? 1 : -1;
  mpfr_set_inf(argopfr, sign);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}
<IMAG_OCT_POLAR>[-+][nN][aA][nN]"."0  {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_nan(argopfr);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}
 /* number radix 16 */
#[xX][-+]?[0-9a-fA-F]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 16);
  return TOKEN_NUMBERZ;
}
(#[xX]#[eE]|#[eE]#[xX])[-+]?[0-9a-fA-F]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberz_new(yytext, 16);
  return TOKEN_NUMBERZ;
}
#[xX][-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+ {
  yytext += 2;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 16);
  return TOKEN_NUMBERQ;
}
(#[xX]#[eE]|#[eE]#[xX])[-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberq_new(yytext, 16);
  return TOKEN_NUMBERQ;
}
(#[xX]#[eE]|#[eE]#[xX])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[xX]#[eE]|#[eE]#[xX])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
#[xX][-+][iI][nN][fF]"."0 {
  int sign = yytext[2] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
#[xX][-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
(#[xX]#[iI]|#[iI]#[xX])[-+]?[0-9a-fA-F]+ {
  yytext += 4;
  if (*yytext == '+') yytext++;
  yylval.obj = numberr_new(yytext, 16);
  return TOKEN_NUMBERR;
}
(#[xX]#[iI]|#[iI]#[xX])[-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+ {
  yylval.obj.type = NUMBERR;
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 16);
  mpfr_init_set_q(yylval.obj.numberr, opq, MPFR_RNDN);
  return TOKEN_NUMBERR;
}
(#[xX]#[iI]|#[iI]#[xX])[-+][iI][nN][fF]"."0 {
  int sign = yytext[4] == '+' ? 1 : -1;
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_inf(yylval.obj.numberr, sign);
  return TOKEN_NUMBERR;
}
(#[xX]#[iI]|#[iI]#[xX])[-+][nN][aA][nN]"."0 {
  yylval.obj.type = NUMBERR;
  mpfr_init(yylval.obj.numberr);
  mpfr_set_nan(yylval.obj.numberr);
  return TOKEN_NUMBERR;
}
 /* real+imag */
#[xX][-+]?[0-9a-fA-F]+[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 16, MPFR_RNDN);    
}
(#[xX]#[eE]|#[eE]#[xX])[-+]?[0-9a-fA-F]+[-+] |
(#[xX]#[iI]|#[iI]#[xX])[-+]?[0-9a-fA-F]+[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 2, MPFR_RNDN);    
}
#[xX][-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[xX]#[eE]|#[eE]#[xX])[-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+[-+] |
(#[xX]#[iI]|#[iI]#[xX])[-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[xX]#[eE]|#[eE]#[xX])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[xX][-+][nN][aA][nN]"."0[-+] |
(#[xX]#[eE]|#[eE]#[xX])[-+][nN][aA][nN]"."0[-+] |
(#[xX]#[iI]|#[iI]#[xX])[-+][nN][aA][nN]"."0[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];
  mpfr_set_nan(real);
}
#[xX][-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[xX]#[iI]|#[iI]#[xX])[-+][iI][nN][fF]"."0[-+] {
  BEGIN IMAG_HEX;
  sign_imag = yytext[yyleng - 1];
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}

<IMAG_HEX>[0-9a-fA-F]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  yylval.obj = numberc_new_r_istr(real, yytext, 16);
  if (sign_imag == '-') {
    mpfr_neg(mpc_imagref(yylval.obj.numberc),mpc_imagref(yylval.obj.numberc),
                          MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
<IMAG_HEX>[0-9a-fA-F]+"/"[0-9a-fA-F]+[iI] {
  BEGIN INITIAL;
  yytext[yyleng - 1] = '\0';
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(opfr, opq, MPFR_RNDN);
  if (sign_imag == '-') {
    mpfr_neg(opfr, opfr, MPFR_RNDN);
  }
  mpc_set_fr_fr(yylval.obj.numberc, real, opfr, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_HEX>[iI] {
  BEGIN INITIAL;
  if (sign_imag == '+') {    
    yylval.obj = numberc_new_r_istr(real, "1", 16);
  } else {
    yylval.obj = numberc_new_r_istr(real, "-1", 16);
  }  
  return TOKEN_NUMBERC;
}

<IMAG_HEX>[-+][iI][nN][fF]"."0[iI] {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;  
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set(mpc_realref(yylval.obj.numberc), real, MPFR_RNDN);
  int sign = sign_imag == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC; 
}
<IMAG_HEX>[nN][aA][nN]"."0[iI] {
  BEGIN INITIAL;
  mpfr_t imag;
  mpfr_set_nan(imag);
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
 /* #x0+imag */
#[xX][-+][iI] {  
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  if (yytext[2] == '+') {
    mpfr_set_si(mpc_imagref(yylval.obj.numberc), 1, MPFR_RNDN);
  } else {
    mpfr_set_si(mpc_imagref(yylval.obj.numberc), -1, MPFR_RNDN);
  }
  return TOKEN_NUMBERC;
}
#[xX][-+][0-9a-fA-F]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 2, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[xX]#[eE]|#[eE]#[xX])[-+][0-9a-fA-F]+[iI] |
(#[xX]#[iI]|#[iI]#[xX])[-+][0-9a-fA-F]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(mpc_imagref(yylval.obj.numberc), yytext, 2, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[xX][-+][0-9a-fA-F]+"/"[0-9a-fA-F]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);  
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
(#[xX]#[eE]|#[eE]#[xX])[-+][0-9a-fA-F]+"/"[0-9a-fA-F]+[iI] |
(#[xX]#[iI]|#[iI]#[xX])[-+][0-9a-fA-F]+"/"[0-9a-fA-F]+[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);  
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(mpc_imagref(yylval.obj.numberc), opq, MPFR_RNDN);
  return TOKEN_NUMBERC;
}
#[xX][-+][iI][nN][fF]"."0[iI]  {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);  
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign =  yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
#[xX][-+][nN][aA][nN]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}
(#[xX]#[iI]|#[iI]#[xX])[-+][iI][nN][fF]"."0[iI] |
(#[xX]#[eE]|#[eE]#[xX])[-+][iI][nN][fF]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);  
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  int sign =  yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(mpc_imagref(yylval.obj.numberc), sign);
  return TOKEN_NUMBERC;
}
(#[xX]#[eE]|#[eE]#[xX])[-+][nN][aA][nN]"."0[iI] |
(#[xX]#[iI]|#[iI]#[xX])[-+][nN][aA][nN]"."0[iI] {
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_ui(mpc_realref(yylval.obj.numberc), 0, MPFR_RNDN);
  mpfr_set_nan(mpc_imagref(yylval.obj.numberc));
  return TOKEN_NUMBERC;
}

 /* polar  */
#[xX][-+]?[0-9a-fA-F]+@ {
  BEGIN IMAG_HEX_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 16, MPFR_RNDN);    
}
(#[xX]#[eE]|#[eE]#[xX])[-+]?[0-9a-fA-F]+@ |
(#[xX]#[iI]|#[iI]#[xX])[-+]?[0-9a-fA-F]+@ {
  BEGIN IMAG_HEX_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpfr_set_str(real, yytext, 16, MPFR_RNDN);    
}
#[xX][-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+@ {
  BEGIN IMAG_HEX_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[xX]#[eE]|#[eE]#[xX])[-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+@ |
(#[xX]#[iI]|#[iI]#[xX])[-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+@ {
  BEGIN IMAG_HEX_POLAR;
  yytext[yyleng - 1] = '\0';
  yytext += 4;
  if (*yytext == '+') yytext++;
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(real, opq, MPFR_RNDN);  
}
(#[xX]#[eE]|#[eE]#[xX])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_HEX_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
#[xX][-+][nN][aA][nN]"."0@ |
(#[xX]#[eE]|#[eE]#[xX])[-+][nN][aA][nN]"."0@ |
(#[xX]#[iI]|#[iI]#[xX])[-+][nN][aA][nN]"."0@ {
  BEGIN IMAG_HEX_POLAR;
  mpfr_set_nan(real);
}
#[xX][-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_HEX_POLAR;
  int sign = yytext[2] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}
(#[xX]#[iI]|#[iI]#[xX])[-+][iI][nN][fF]"."0@ {
  BEGIN IMAG_HEX_POLAR;
  int sign = yytext[4] == '+' ? 1 : -1;
  mpfr_set_inf(real, sign);
}

<IMAG_HEX_POLAR>[-+]?[0-9a-fA-F]+  {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpfr_set_str(argopfr, yytext, 16, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_HEX_POLAR>[-+]?[0-9a-fA-F]+"/"[0-9a-fA-F]+ {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  if (*yytext == '+') { yytext++; }
  mpq_set_str(opq, yytext, 16);
  mpfr_set_q(argopfr, opq, MPFR_RNDN);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;
}
<IMAG_HEX_POLAR>[-+][iI][nN][fF]"."0 {
 BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  int sign = yytext[0] == '+' ? 1 : -1;
  mpfr_set_inf(argopfr, sign);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}
<IMAG_HEX_POLAR>[-+][nN][aA][nN]"."0  {
  BEGIN INITIAL;
  yylval.obj.type = NUMBERC;
  mpc_init2(yylval.obj.numberc, MPC_PREC);
  mpfr_set_nan(argopfr);
  mpfr_sin_cos(sopfr, copfr, argopfr, MPFR_RNDN);
  mpfr_mul(imag, real, sopfr, MPFR_RNDN);
  mpfr_mul(real, real, copfr, MPFR_RNDN);
  mpc_set_fr_fr(yylval.obj.numberc, real, imag, MPC_RNDNN);
  return TOKEN_NUMBERC;    
}

#\\{any_character} {
  yytext += 2;
  yylval.obj = character_new(yytext);
  return TOKEN_CHARACTER;
}
 /* character name */
#\\alarm {  yylval.obj = character_new("\x7"); return TOKEN_CHARACTER;  }
#\\backspace {  yylval.obj = character_new("\x8"); return TOKEN_CHARACTER; }
#\\delete {  yylval.obj = character_new("\x7f"); return TOKEN_CHARACTER;  }
#\\escape {  yylval.obj = character_new("\x1b"); return TOKEN_CHARACTER;  }
#\\newline {  yylval.obj = character_new("\xa"); return TOKEN_CHARACTER;  }
#\\null {  yylval.obj = character_new("\x0"); return TOKEN_CHARACTER;  }
#\\return {  yylval.obj = character_new("\xd"); return TOKEN_CHARACTER;  }
#\\space {  yylval.obj = character_new(" "); return TOKEN_CHARACTER;  }
#\\tab {  yylval.obj = character_new("\x9"); return TOKEN_CHARACTER;  }
#\\[xX][0-9a-fA-F]+ {
  yytext += 3;
  char *endptr = NULL;
  yylval.obj = (Object){.type=CHARACTER,
                        .character=strtol(yytext, &endptr, 16)};
  return TOKEN_CHARACTER;
}

 /* string */
 \"\" { yylval.obj = string_empty; return TOKEN_STRING_EMPTY; }
\" {
  BEGIN STRING_ELEMENT;
}
<STRING_ELEMENT>\" {
  BEGIN INITIAL;
  yylval.obj = string_empty;
  return TOKEN_STRING_EMPTY;
}
<STRING_ELEMENT>{any_character_other_than_quote_backslash} {
  yylval.obj = character_new(yytext);
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\a {
  yylval.obj = character_new("\a");
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\b {
  yylval.obj = character_new("\b");
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\t {
  yylval.obj = character_new("\t");
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\n {
  yylval.obj = character_new("\n");
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\r {
  yylval.obj = character_new("\r");
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\\" {
  yylval.obj = character_new("\"");
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\\\ {
  yylval.obj = character_new("\\");
  return TOKEN_STRING_ELEMENT;
}
<STRING_ELEMENT>\\{intraline_whitespace}*{line_ending}{intraline_whitespace}* {}
<STRING_ELEMENT>\\[xX][0-9a-fA-F]+";" {
  yytext[yyleng - 1] = '\0';
  yytext += 2;
  char *endptr = NULL;
  gunichar c = strtol(yytext, &endptr, 16);
  yylval.obj = (Object){.type=CHARACTER, .character=c};
  return TOKEN_STRING_ELEMENT;
}
"(" |
")" |
"'" |
"`" |
"," |
"." { return yytext[0]; }

"#(" { return TOKEN_SHARP_OP; }
"#u8(" { return TOKEN_SHARP_U_EIGHT_OP; }
<<EOF>> {
  if (interactive_mode) {
    exit(0);
  }
  yylval.obj = (Object){.type=EOF_OBJ};
  return TOKEN_EOF_OBJ;
}
. {
  yyunput(1, yytext);
  input();
  printf("Mystery character '%c'(\\x%x)\n", *yytext, *yytext);
}
%%

#include <stdbool.h>
bool syntax_error = true;
void yyerror(char const *msg) {
  if (syntax_error) {
    printf("%d: %s at '%s'\n", yylineno, msg, yytext);
  }
yylineno = 0;
}
int stringto(char *str) {
  YY_BUFFER_STATE bp = yy_scan_string(str);
  yy_switch_to_buffer(bp);
  syntax_error = false;
  int n = yyparse();
  syntax_error = true;
  if (input() != EOF) {
    n = 1;
  }
  yy_delete_buffer(bp);
  return n;
  }

