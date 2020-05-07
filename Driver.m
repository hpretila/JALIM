clear

% TURN OFF ALL WARNINGS (OCTAVE)
warning('off','all');

% IMPORT
addpath ("Lexer");
addpath ("Runtime");
addpath ("Helper");

disp ("======================================================================");
disp ("UNIT TESTING SCALAR FUNCTIONS");

% ======================================================================
% Debug
% ----------------------------------------------------------------------
% TEST OUT Debug.testFunc
disp ("testing <Debug.testFunc>.");

% SO THAT I CAN COLLAPSE THIS CODE FOR READABILITY
if true
    n = 1;
    failures = 0;

    % Expecting n = 2, testLog = "Test (1) was unsuccessful.";, failures = 1, 
    % isFail = 1
    [n, testLog, failures, isFail] = Debug.testFunc (n, failures, ...
        2, 3);

    if ((n == 2 && strcmp (testLog, "Test (1) was unsuccessful. Expected[1 1] <3> but got[1 1] <2>.")) ... 
            && failures == 1) && isFail == 1
        disp ("Test (1) was successful.");
    else
        disp ("Test (1) was unsuccessful.");
    end

    % Expecting n = 3, testLog = "Test (2) was unsuccessful.";, failures = 2, 
    % isFail = 1
    [n, testLog, failures, isFail] = Debug.testFunc (n, failures, ...
        "foobar", "notfoobar");

    if ((n == 3 && strcmp (testLog, "Test (2) was unsuccessful. Expected [1 9] <notfoobar> but got [1 6] <foobar>.")) ...
            && failures == 2) && isFail == 1
        disp ("Test (2) was successful.");
    else
        disp ("Test (2) was unsuccessful.");
    end

    % Expecting n = 4, testLog = "Test (3) was successful.";, failures = 2, 
    % isFail = 0
    [n, testLog, failures, isFail] = Debug.testFunc (n, failures, ...
        "foobar", "foobar");

    if ((n == 4 && strcmp (testLog, "Test (3) was successful.")) && failures == 2) && ...
            isFail == 0
        disp ("Test (3) was successful.");
    else
        disp ("Test (3) was unsuccessful.");
    end
end

% ======================================================================
% StringHelper
% ----------------------------------------------------------------------
% TEST OUT StringHelper.char2str

disp ("testing <StringHelper.cellToChar>.");

if true
    n = 1;
    failures = 0;
    %x = StringHelper.char2Str (importdata("Test/program.jalim"))
    %disp(size(StringHelper.char2Str (importdata("Test/program.jalim"))))

    % Expecting n = 2, testLog = "Test (1) was unsuccessful.";, failures = 1, 
    % isFail = 1
    % OCTAVE DOES NOT SUPPORT 'newline'
    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
        StringHelper.char2Str (importdata("Test/cleantest.jalim")), ...
        ['void main () {' ...
    char (10) '    fprintf ("Test 1 passed. FUNCTIONS AND STANDARD METHOD IMPLEMENTED.");' ...
    char (10) '    if ("cake" == "cake") {' ... 
    char (10) '        fprintf ("Test 2 Conditionals. FUNCTIONS AND STANDARD METHOD IMPLEMENTED.");' ...
    char (10) '    }' ...
    char (10) '}' ...
    char (10) 'main (); //EDGE' ...
    char (10)]);
        disp (testLog);
end

% ----------------------------------------------------------------------
% TEST OUT StringHelper.cell2CodeStr

disp ("testing <StringHelper.cell2CodeStr>.");

if true
    n = 1;
    failures = 0;
    %y = StringHelper.cell2CodeStr (importdata("Test/program.jalim"))
    %disp(size(StringHelper.cell2CodeStr (importdata("Test/program.jalim"))))

    % Expecting n = 2, testLog = "Test (1) was unsuccessful.";, failures = 1, 
    % isFail = 1
    % OCTAVE DOES NOT SUPPORT 'newline'
    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
        StringHelper.cell2CodeStr (importdata("Test/cleantest.jalim")), ...
        ['void main () {' char (182) '    fprintf ("Test 1 passed. FUNCTIONS AND STANDARD METHOD IMPLEM' ...
            'ENTED.");' char (182) '    if ("cake" == "cake") {' char (182) '        fprintf ("Test 2 Conditionals.' ...
            ' FUNCTIONS AND STANDARD METHOD IMPLEMENTED.");' char (182) '    }' char (182) '}' char (182) 'main (); //EDGE' char (182) '']);
        disp (testLog);
end

% ======================================================================
% Language

% ----------------------------------------------------------------------
% TEST OUT Language.endExpression
disp ("testing <Language.endExpression>.");

if true
    n = 1;
    failures = 0;

    % RUN THE FUNCTION FIRST
    [endOfExpression, terminalExpression] = Language.endExpression ();

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    [endOfExpression, terminalExpression], ...
        [true, ';']);
        disp (testLog);
end

% ----------------------------------------------------------------------
% TEST OUT Language.isOperator
disp ("testing <Language.classifyLexeme>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ("=="), EnumLexeme.LEX_OPERATOR());
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ("="), EnumLexeme.LEX_ASSIGN());
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ("int"), EnumLexeme.LEX_IDENT());
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme (''), EnumLexeme.LEX_EMPTY());
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ("varName"), EnumLexeme.LEX_VARS());
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ('true'), EnumLexeme.LEX_BOOL());
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ('147'), EnumLexeme.LEX_DIGITS());
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ('"Hello, World"'), EnumLexeme.LEX_STRINGS());
        disp (testLog); 
            
    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.classifyLexeme ('return'), EnumLexeme.LEX_IGNORE());
        disp (testLog);  
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isOperator
disp ("testing <Language.isOperator>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("=="), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("<="), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator (">="), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("="), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("<"), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator (">"), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("/"), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("*"), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("+"), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isOperator ("-"), true);
        disp (testLog);  
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isOperator
disp ("testing <Language.isString>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isString ('"Hello, world"'), true);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isString ("<="), false);
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isAssignment
disp ("testing <Language.isAssignment>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isAssignment ('"Hello, world"'), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isAssignment ("="), true);
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isBool
disp ("testing <Language.isBool>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isBool ('"Hello, world"'), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isBool ("true"), true);
        disp (testLog);    

        
    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isBool ("false"), true);
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isDigit
disp ("testing <Language.isDigit>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isDigit ('"Hello, world"'), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isDigit ("1234556"), true);
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isLinker
disp ("testing <Language.isLinker>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isLinker ('"Hello, world"'), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isLinker ("#include <WOahAHHAh>"), true);
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isIdentifier
disp ("testing <Language.isIdentifier>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isIdentifier ('"Hello, world"'), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isIdentifier ("int"), true);
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isScopeInitiator
disp ("testing <Language.isScopeInitiator>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isScopeInitiator ('"Hello, world"'), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isScopeInitiator ("{"), true);
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.isScopeTerminator
disp ("testing <Language.isScopeTerminator>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isScopeTerminator ('"Hello, world"'), false);
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.isScopeTerminator ("}"), true);
        disp (testLog);    
end  


% ----------------------------------------------------------------------
% TEST OUT Language.terminatorFromInitiator
disp ("testing <Language.terminatorFromInitiator>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.terminatorFromInitiator ('('), ')');
        disp (testLog);    

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.terminatorFromInitiator ("}"), '');
        disp (testLog);    
end  

% ----------------------------------------------------------------------
% TEST OUT Language.cleanCode
% BRACKETS SCOPE, ASSIGNMENT AND EQUATION
disp ("testing <Language.cleanCode>.");

if true
    n = 1;
    failures = 0;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.cleanCode ('void cake( int abcdefg == 55 )'), ...
        ['void' char (187) 'cake' char (187) '(int' char (187) 'abcdefg' char (187) ...
            '==' char (187) '55)' char (187)]);
        disp (testLog);

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
        Language.cleanCode ('          ( abcdefg= 55){'), ...
        ['(abcdefg' char (187) '=' char (187) '55)' char (187) '{']);
        disp (testLog);

    [~, testLog, ~, ~] = Debug.testFunc (n, failures, ...
    Language.cleanCode ('5*2 <3+ 6'), ...
        ['5' char (187) '*' char (187) '2' char (187) '<' char (187) '3' char (187) ...
            '+' char (187) '6']);
        disp (testLog);
end

% ----------------------------------------------------------------------
% TEST OUT Language.spaghettiCodeSeparation
% BRACKETS SCOPE, COMMENTS AND EQUATION
disp ("testing <Language.Debug.testFunc>.");

if true
    n = 1;
    failures = 0;

    % FORM TEST
    % INITIALISE CODE
    rCodeArray(1) = SpaghettiExpression;

    % POPULATE CODEBASE
    rCode0 = SpaghettiExpression;
    rCode0.SetCode(['#include' char (187) '<' char (187) 'stdio.h' char (187) '>' char (187)], 7);
    rCode1 = SpaghettiExpression;
    rCode1.SetCode(['int' char (187) 'main' char (187) '()' char (187) '{' char (187) '}' char (187)], 0);
    rCode2 = SpaghettiExpression;
    rCode2.SetCode(['otherBoi' char (187) '();'], 0);
    rCode3 = SpaghettiExpression;
    rCode3.SetCode(['jeff' char (187) '();'], 0);
    rCodeArray (1) = rCode0;
    rCodeArray (2) = rCode1;
    rCodeArray (3) = rCode2;
    rCodeArray (4) = rCode3;

    [n, testLog, failures, ~] = Debug.testFunc (n, failures, ...
    Language.spaghettiGetExpression(StringHelper.cell2CodeStr(['#include<stdio.h>\nint main () { \n}\notherBoi();/*COMMENT*/jeff();'])), ...
        rCodeArray);
        disp (testLog);
end


disp ("======================================================================");
disp ("NOW TESTING INTERPRETER");

% ======================================================================
% TEST OUT RUNTIME FEATURES

% ----------------------------------------------------------------------
% TEST OUT Language.spaghettiCodeSeparation
% BRACKETS SCOPE, COMMENTS AND EQUATION
disp ("testing <JALIM RUNTIME ROUTINE>.");
spaghettiRuntime = SpaghettiRuntime;
spaghettiRuntime.Init();
spaghettiRuntime.ParseFile("Test/test.jalim");

% ----------------------------------------------------------------------
% TEST OUT THROW
disp ("testing <Debug.Throw>.");
Debug.Throw ('Test (1) was successful.')