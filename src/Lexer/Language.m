% CODEBASE FOR LANGUAGE DECODING
classdef Language
    methods (Static)
        % THIS FUNCTION WILL CLEAN CODE TO MAKE IT EASIER FOR LATER FUNCTIONS TO WORK.
        % Checked with Driver.
        function o = cleanCode (codeIn)
            o = codeIn;
            if (iscell(o))
                % CLEAN UP CODE IF IT'S IN CELL FORM; USE STRING HELPER
                addpath ("/Helper");
            end

            
            % REPLACE ALL MACRON SYMBOLS WITH SPACES
            o = strrep (o, char(187), ' ');
            
            % MAKE SURE ALL BRACKETS ARE SPACED.
            o = strrep (o, ' (', '(');
            o = strrep (o, '( ', '(');
            o = strrep (o, '(', ' (');
            o = strrep (o, ' )', ')');
            o = strrep (o, ') ', ')');
            o = strrep (o, ')', ') ');
            % Scope
            o = strrep (o, ' {', '{');
            o = strrep (o, '{ ', '{');
            o = strrep (o, '{', ' {');
            o = strrep (o, '} ', '}');
            o = strrep (o, ' }', '}');
            o = strrep (o, '}', '} ');

            % MAKE SURE ALL COMMAS ARE NOT SPACED
            o = strrep (o, ' ,', ',');
            o = strrep (o, ', ', ',');
            o = strrep (o, ',', ' , ');
            
            % MAKE SURE ALL OPERATORS ARE SPACED.
            % Assignment
            o = strrep (o, ' =', '=');
            o = strrep (o, '= ', '=');
            o = strrep (o, '=', ' = ');
            % Less than
            o = strrep (o, ' <', '<');
            o = strrep (o, '< ', '<');
            o = strrep (o, '<', ' < ');
            % Greater than
            o = strrep (o, ' >', '>');
            o = strrep (o, '> ', '>');
            o = strrep (o, '>', ' > ');
            % NEQ
            o = strrep (o, '! =', '!=');
            o = strrep (o, ' !  = ', '!=');
            o = strrep (o, '!=', ' != ');
            % Equation
            o = strrep (o, ' =  = ', '==');
            o = strrep (o, '==', ' == ');
            % Less than and equal to
            o = strrep (o, ' < = ', '<=');
            o = strrep (o, '<=', ' <= ');
            % More than and equal to
            o = strrep (o, ' > = ', '>=');
            o = strrep (o, '>=', ' >= ');
            % Addition
            o = strrep (o, ' +', '+');
            o = strrep (o, '+ ', '+');
            o = strrep (o, '+', ' + ');
            % Subtraction
            o = strrep (o, ' -', '-');
            o = strrep (o, '- ', '-');
            o = strrep (o, '-', ' - ');
            % Multiplication
            o = strrep (o, ' *', '*');
            o = strrep (o, '* ', '*');
            o = strrep (o, '*', ' * ');
            % Division
            o = strrep (o, ' /', '/');
            o = strrep (o, '/ ', '/');
            o = strrep (o, '/', ' / ');
            % Commas
            o = strrep (o, ' ,', ',');
            o = strrep (o, ', ', ',');
            o = strrep (o, ',', ', ');

            % REMOVE SPACES
            o = regexprep(o,'\s+',' ');

            % REMOVE SPACES IN COMMENT IDENTIFIERS
            o = strrep (o, '/ /', '//');
            o = strrep (o, '/ *', '/*');
            o = strrep (o, '* /', '*/');
            
            % o = (strrep (o,'\n',char(187)));
            
            % REMOVE SPACES
            o = regexprep(o,'\s+',' ');
            o = strrep (o, ' ;', ';');

            % REPLACE ALL SPACES WITH MACRON SYMBOL
            o = strrep (o, ' ', char(187));
            
            if (length(o) ~= 0)
                if (strcmp(o(1),char(187)))
                    o(1)='';
                end
            end

            o = char(o);
        end

        % ======================================================================
        % GRAMMAR ANALYSIS

        % PROCESSES CODE PER GRAPHEME, OBTAINING SCOPES, AND IF CURRENT GRAPHEME IS END OF EXPRESSION
        function [codeDepth, codeType, endOfExpression, terminalExpression, strCode] = processExpressionByGrapheme ...
             (in_codeDepth, in_codeType, in_strCode, cursorCurrent, in_terminalExpression)

            % addpath "./Helper"
            
            % PROVIDE DEFAULT VALUES
            codeDepth = in_codeDepth;
            codeType = in_codeType;
            terminalExpression = StringHelper.cell2CodeStr(in_terminalExpression);
            endOfExpression = false;
            strCode = in_strCode;
            
            % GET CURRENT CURSOR VALUE
            cursorVal = strCode (cursorCurrent);
            
            % ----------------------------------------------------------------------
            % DEBUG CODE
            try 
                debug_StrCode = strcat('  ', strCode, char(187));
                disp_StrCode = strcat('Size <', mat2str(size(strCode)), '>:', debug_StrCode(2:cursorCurrent), '~~[ ', debug_StrCode(cursorCurrent+1), ', [', debug_StrCode(cursorCurrent+2), ...
                 '] ]~~', debug_StrCode(cursorCurrent +3:length(debug_StrCode)), '\n');

                Debug.Log (disp_StrCode)
                Debug.LogLine ("---------------------------------------");
                Debug.Log (strcat('cursorCurrent: ', mat2str(cursorCurrent), '\n'));
                if cursorCurrent ~= 1 
                    Debug.Log (strcat('current char: ', (strCode(cursorCurrent)), '\n'));
                    Debug.Log (['current two-char pair: ' (strCode(cursorCurrent-1:cursorCurrent)) '\n']);
                    Debug.Log (['comment */ ended: ' mat2str(strcmp (strCode(cursorCurrent-1:cursorCurrent), '*/')) '\n']);
                    Debug.Log (['type: ' mat2str(codeType) '\n']);
                else
                    Debug.Log (['current char: ' (strCode(cursorCurrent)) '\n']);
                end
                Debug.Log (['in_terminalExpression: ' in_terminalExpression '\n']);
                Debug.LogLine ("---------------------------------------");
            catch
                
            end
            
            % ----------------------------------------------------------------------

            % 0 = NORMAL, DIVIDE BY ';'
            % 1 = SUBSCOPE, INLINE FUNCTION ARGUMENT, DIVIDE BY ')' AND WILL SKIP ARGUMENTS
            % 2 = SCOPED CODE, DIVIDE BY '}' AND WILL SKIP SCOPED CODE
            % 3 = COMMENTS, DIVIDE BY '*/' INITIATED BY '/*'
            % 4 = LINKERCODE1, DIVIDE BY '\n' INITIATED BY '#include'
            % 5 = (UNIMPLEMENTED) LINKERCODE2, DIVIDE BY '#enddef' INITIATED BY '#ifdef'
            
            % READ THE CODE, IF ITS CODE DEPTH IS 0 AND THE CURSORVAL ISN'T THE TERMINAL EXPRESSION.
            % A WHOLE SCOPE IS CONSIDERED A SINGULAR LEXEME.

            % FIRST IF CURRENT VALUE IS A SCOPE IDENTIFIER, SHIFT CODEDEPTH.
            % DOWNSHIFT COMES BEFORE CHECK AND UPSHIFT COMES AFTER CHECK.
            if strcmp (cursorVal, '}') && codeType ~= EnumCode.CODE_COMMENTS()
                Debug.LogLine('SCOPE CLOSED')
                codeDepth = in_codeDepth - 1;
            end
            
            if codeDepth == 0 && ~strcmp (cursorVal, terminalExpression) 
                % SPECIAL CASES, SCOPES AND COMMENTS IN NON-COMMENT MODE
                if codeType == EnumCode.CODE_GENERIC()
                    % IF '/*' or '//' COMMENT MODE
                    if cursorCurrent+1 <= length(strCode)
                        if (strcmp (strCode(cursorCurrent:cursorCurrent+1), '//') || ...
                            strcmp (strCode(cursorCurrent:cursorCurrent+1), '/*'))
                            % SET CURRENT CODE AS COMMENTED CODE
                            codeType = EnumCode.CODE_COMMENTS();
                            terminalExpression = '*/';
                            Debug.LogLine (['terminalExpression set to ' terminalExpression]);

                            % IF IT'S A ONE LINE COMMENT
                            if strcmp (strCode(cursorCurrent:cursorCurrent+1), '//')
                                terminalExpression = char (182);
                            end
                        % IF BEGINNING OF SCOPED CODE 
                        elseif cursorVal == '{'
                            codeType = EnumCode.CODE_SCOPED();
                            terminalExpression = '}';
                            Debug.LogLine (['terminalExpression set to ' terminalExpression]);
                        % NOT YET IMPLEMENTED
                        elseif cursorVal == '#'
                            codeType = EnumCode.CODE_LINKER_1();
                            terminalExpression = char (182);
                            Debug.LogLine (['terminalExpression set to ' terminalExpression]);
                        else
                            terminalExpression = ';';
                            Debug.LogLine (['terminalExpression set to ' terminalExpression]);
                        end 
                    else
                        % WE'RE AT THE LAST CHARACTER. IF IT'S NOT A
                        % TERMINATING CHARACTER, IT COULD BE A COMMENT OR
                        % AN UNCLOSED SCOPE. THE EXPRESSION WILL END HERE.
                        [endOfExpression, terminalExpression] = Language.endExpression ();
                    end
                else
                    % FIRST, DEAL WITH MULTILINE COMMENTS WHICH TERMINATE ON */
                    if codeType == EnumCode.CODE_COMMENTS() ...
                        && strcmp (terminalExpression, '*/') 
                        Debug.LogLine (['terminalExpression set to ' terminalExpression ' and is enabled for scope termination.']);

                        if strcmp (strCode(cursorCurrent-1:cursorCurrent), '*/')
                            [endOfExpression, terminalExpression] = Language.endExpression ();
                        else
                            Debug.LogLine (['Not applicable with ' strCode(cursorCurrent-1:cursorCurrent) '.'])
                        end
                    end
                end
                
                % IF IT'S NOT A TERMINATING VALUE AND IT'S A LINEBREAK, REPLACE WITH SPACE
                if strcmp (cursorVal, char (182))
                    strCode (cursorCurrent) = char(187);
                end
            % IF IT'S A TERMINATING VALUE AND IT'S AT ROOT SCOPE, TERMINATE EXPRESSION
            elseif strcmp (cursorVal, terminalExpression) && codeDepth == 0
                if strcmp (cursorVal, char (182))
                    strCode (cursorCurrent) = char(187);
                end

                Debug.LogLine ("NATURAL TERMINATION");
                [endOfExpression, terminalExpression] = Language.endExpression ();
            end

            % UPSHIFT ONCE CODE HAS CHECKED THE ROOT.
            if strcmp (cursorVal, '{') && codeType ~= EnumCode.CODE_COMMENTS()
                Debug.LogLine('SCOPE NOT CLOSED')
                codeDepth = in_codeDepth + 1;
            end
            
            % ----------------------------------------------------------------------
            % DEBUG CODE

            Debug.LogLine ("---------------------------------------");
            Debug.Log (['endOfExpression: ' mat2str(endOfExpression) '\n']);
            Debug.Log (['codeDepth: ' mat2str(codeDepth) '\n']);
            Debug.Log (['terminalExpression: ' terminalExpression '\n']);
            Debug.LogLine ("=======================================");
            
            % ----------------------------------------------------------------------
        end

        % REFACTORING EXPRESSION TERMINATION CODE
        function [endOfExpression, terminalExpression] = endExpression ()
            Debug.LogLine ("An expression ended");

            % RESET CODE TYPE AND EXPRESSION TERMINATION
            endOfExpression = true;
            terminalExpression = ';';
            Debug.LogLine (['terminalExpression set to ' terminalExpression]);
        end

        % THIS WILL PROCESS THE CODE PER LEXEME, AND WILL TRY TO FIND THE EXPRESSION TYPE THROUGH THE LEXEME USED
        % RECEIVES STRING INPUT codeIn
        % RECEIVES CODETYPE codeType
        % OUTPUTS INT OUTPUT expressionType 
        % EXPRESSION GUIDE
        % 0 - EXPRESSION EXECUTION Where code is run, to return a value if not void
        % 1 - LHSRHS INITIALISATION Where assignment and object creation is done
        % 1 - 2 - LHSRHS ASSIGNMENT Where assignment is done to an object (LHS) and a scope (RHS)
        % 1 - 3 - LHSRHS (RETURN) COMPARISON Where an operator is used on two scopes (LHS,RHS) to return a 0,1 value
        % 4 - SCOPED CODE var funcname () {code}
        % 5 - CONDITIONAL SCOPED CODE if (bool)
        % OUTPUTS INT OUTPUT isComplex 
        function [expressionType] = processExpressionByLexeme (codeIn, codeType)
            % CONVERT TO CHARACTER
            codeIn = char(codeIn);
            switch codeType 
                % UNSCOPED CODE
                case EnumCode.CODE_GENERIC()
                    % SPLIT CODE INTO KEYWORDS
                    splitCode = strsplit(codeIn);

                    if 0 < length(splitCode)
                        % CHECK PER LEXEME
                        for lexeme = splitCode
                            % DEDUCES THE EXPRESSION TYPE BASED ON WHAT LEXEMES ARE FOUND
                            % TODO: REPLACE WITH DYNAMIC KEYWORD CHECKER
                            % INITIALISATIONS HAVE PRECEDENCE OVER ASSIGNMENTS
                            if Language.isIdentifier (lexeme)
                                expressionType = 1;

                                % INITIALISATIONS ARE NEVER USED IN CONDITIONALS OR OPERATIONS.
                                break;

                            else
                                % DEFAULT CODE IS EXPRESSION
                                % expressionType = 0;

                                % COMPARISONS
                                if Language.isOperator (lexeme)
                                    expressionType = 3;

                                % ASSIGNMENTS HAVE PRECEDENCE OVER COMPARISONS, 
                                % THEREFORE, VALIDATION LOOP IS STOPPED HERE.
                                elseif Language.isAssignment (lexeme)
                                    expressionType = 2;
                                    break;

                                % OTHERWISE, THE EXPRESSION IS JUST EXECUTION, PROBABLY A REFERENCE.
                                else
                                    expressionType = 0;
                                end

                            end
                        end
                    end
                case EnumCode.CODE_SCOPED()
                    % SPLIT CODE INTO KEYWORDS
                    splitCode = strsplit(codeIn);

                    if 0 < length(splitCode)
                        % CHECK PER LEXEME
                        for lexeme = splitCode
                            % DEDUCES THE EXPRESSION TYPE BASED ON WHAT LEXEMES ARE FOUND
                            % TODO: REPLACE WITH DYNAMIC KEYWORD CHECKER
                            if strcmp(lexeme(1), "{")
                                % DEFAULT, IT IS SCOPED CODE
                                expressionType = EnumExpression.EXP_SCOPED(); % SET TO BODY SCOPED CODE
                            
                            % FOR OTHER CASES, CODE EXECUTION
                            else
                                expressionType = EnumExpression.EXP_GENERIC(); % SET TO GENERIC CODE                            
                                break;
                            end
                        end
                    end
            end
        end

        % CLASSIFIES LEXEME; RETURNING TYPE
        function readType = classifyLexeme (currentLexeme)
            % CHECK IF IT IS EMPTY
            if 0 == length(char(cellstr(currentLexeme))) && strcmp ('', currentLexeme) 
                readType = EnumLexeme.LEX_EMPTY(); % IT'S EMPTY!
            % CHECK FOR IDENTIFIERS
            elseif Language.isIdentifier (currentLexeme)
                readType = EnumLexeme.LEX_IDENT(); % TREAT AS IDENTIFIER
            % CHECK FOR ASSIGNMENT
            elseif Language.isAssignment (currentLexeme)
                readType = EnumLexeme.LEX_ASSIGN(); % TREAT AS ASSIGNMENT
            % CHECK FOR OPERATORS
            elseif Language.isOperator (currentLexeme)
                readType = EnumLexeme.LEX_OPERATOR(); % TREAT AS OPERATOR
            % CHECK FOR NUMBERS 
            elseif Language.isDigit (currentLexeme)
                readType = EnumLexeme.LEX_DIGITS(); % TREAT AS DIGITS
            % CHECK FOR BOOLEANS 
            elseif Language.isBool (currentLexeme)
                readType = EnumLexeme.LEX_BOOL(); % TREAT AS DIGITS
            % CHECK FOR STRING 
            elseif Language.isString (currentLexeme)
                readType = EnumLexeme.LEX_STRINGS(); % TREAT AS DIGITS
            % CHECK FOR RETURN
            elseif 0 < length(char(cellstr(currentLexeme))) && strcmp ('return', currentLexeme) 
                readType = EnumLexeme.LEX_IGNORE();
            % IF NOT NULL
            else
                readType = EnumLexeme.LEX_VARS(); % DEFAULT AS VAR/METHODNAMES
            end
        end

        % RETURNS IF A STRING IS AN OPERATOR
        function o = isOperator (lexeme)
            if strcmp(lexeme, "==") || ...
                strcmp(lexeme, "!=") || ...
                strcmp(lexeme, "<=") || ...
                strcmp(lexeme, ">=") || ...
                strcmp(lexeme, "<") || ...
                strcmp(lexeme, ">") || ...
                strcmp(lexeme, "/") || ...
                strcmp(lexeme, "*") || ...
                strcmp(lexeme, "+") || ...
                strcmp(lexeme, "-")
                o = true;
            else
                o = false;
            end
        end

        % RETURNS IF A STRING IS A STRING LITERAL
        function o = isString (lexeme)
            lexeme = char(lexeme);
            lexemeLength = length(lexeme);

            % IF THE LAST AND FIRST PARTS ARE DOUBLE QUOTES
            if strcmp(lexeme (1), '"') && strcmp(lexeme (lexemeLength), '"')
                o = true;
            else
                o = false;
            end
        end

        % RETURNS IF A STRING IS AN ASSIGNMENT
        function o = isAssignment (lexeme)
            if strcmp(lexeme, "=") || ...
                strcmp(lexeme, "+=") || ... % NOT IMPLEMENTED
                strcmp(lexeme, "-=") % NOT IMPLEMENTED
                o = true;
            else
                o = false;
            end
        end

        % RETURNS IF A LEXEME STRING IS A BOOLEAN
        function o = isBool (lexeme)
            if strcmp(lexeme, "true") || ...
                strcmp(lexeme, "false")
                o = true;
            else
                o = false;
            end
        end

        % RETURNS IF A LEXEME STRING IS A DIGIT
        function o = isDigit (lexeme)
            % IF STR2DOUBLE RESULT IS NOT UNDEFINED, IT'S A NUMBER, AND IMAGINARY NUMBERS ARE NOT COUNTED
            if ~isnan(str2double(lexeme)) && ~strcmp (lexeme, 'i')
                o = true;
            else
                o = false;
            end
        end
        
        % RETURNS IF A LEXEME STRING IS LINKER CODE
        function o = isLinker (lexeme)
            % IF STR2DOUBLE RESULT IS NOT UNDEFINED, IT'S A NUMBER
            lexeme = char(lexeme);
            if lexeme (1) == '#'
                o = true;
            else
                o = false;
            end
        end
        
        % RETURNS IF A STRING IS AN IDENTIFIER
        function o = isIdentifier (lexeme)
            if strcmp(lexeme, "int") || ...
                strcmp(lexeme, "float") || ...
                strcmp(lexeme, "void") || ...
                strcmp(lexeme, "bool") || ...
                strcmp(lexeme, "char") || ...
                strcmp(lexeme, "string") || ...
                strcmp(lexeme, "struct")
                o = true;
            else
                o = false;
            end
        end

        % RETURNS IF A CHARACTER IS A SCOPE INITIATOR
        function o = isScopeInitiator (charTerm)
            if strcmp(charTerm, "(") || ...
                strcmp(charTerm, "{") || ...
                strcmp(charTerm, "[")
                o = true;
            else
                o = false;
            end
        end

        % RETURNS IF A CHARACTER IS A SCOPE TERMINATOR
        function o = isScopeTerminator (charTerm)
            if strcmp(charTerm, ")") || ...
                strcmp(charTerm, "}") || ...
                strcmp(charTerm, "]")
                o = true;
            else
                o = false;
            end
        end

        % RETURNS A CORRESPONDING CHAR TERMINATOR FROM INITIATOR
        function terminatingBracket = terminatorFromInitiator (charInit)
            terminatingBracket = '';
            Debug.LogLine (['ATTEMPTING TO FIND TERMINATOR FOR ' charInit])
            if ~strcmp(charInit, '')
                % SCOPE IS INITIATED
                switch charInit
                    case '('
                        terminatingBracket = ')';
                    case '{'
                        terminatingBracket = '}';
                    case '['
                        terminatingBracket = ']';
                end
            else
                Debug.LogLine (['VALUE IS INVALID.'])
            end
        end


        % ======================================================================
        % RAW CODE CONVERSION
        
        % THIS WILL SPLIT STRINGS IN ACCORDANCE
        % TO ONE OF THE FOLLOWING:
        % - HEADER IMPORTS
        % - INSTRUCTIONS (pure code)
        % - INSTRUCTIONS W/ SCOPES (functions with code)
        % - CONDITIONALS W/ SCOPES (for, if, etc)
        % CONVERTS STRING INTO EXPRESSION, SpaghettiExpression, WITH INFORMATION REGARDING SCOPES
        % Checked with Driver.
        function o = spaghettiGetExpression (codeIn)
            Debug.SpecialLogLine (['======================================='])
            Debug.SpecialLogLine (['ATTEMPTING TO READ STRING'])
            Debug.SpecialLogLine (strcat('<', char(codeIn), '>'))

            % INITIALIZE VARIABLES
            strCode_ = Language.cleanCode(codeIn);
            strCode = char(cellstr(strCode_));

            interpretedCode(1) = SpaghettiExpression;
            nthInterpretedCode = 0;
            
            % READ STRING CHARACTER BY CHARACTER
            % FIRST CHARACTER OF A SINGLE INSTRUCTION
            cursorFirst = 1; 
            endOfExpression = false; % false

            % 0 = NORMAL, DIVIDE BY ';'
            % 1 = FUNCTION ARGUMENT, DIVIDE BY ')' AND WILL SKIP ARGUMENTS
            % 2 = SCOPED CODE, DIVIDE BY '}' AND WILL SKIP SCOPED CODE
            % 3 = COMMENTS, DIVIDE BY '\n' INITIATED BY '//'
            % 4 = LINKERCODE1, DIVIDE BY '\n' INITIATED BY '#include'
            % 5 = (UNIMPLEMENTED) LINKERCODE2, DIVIDE BY '#enddef' INITIATED BY '#ifdef'
            codeType = EnumCode.CODE_GENERIC();
            codeDepth = 0;
            terminalExpression = ';'; % Where to end code

            for cursorCurrent = 1:length(strCode)
                % WILL CHECK IF THE CURRENT CODE INDICATES THE END OF EXPRESSION
                [codeDepth, codeType, endOfExpression, terminalExpression, strCode]  = ...
                 Language.processExpressionByGrapheme (codeDepth, codeType, strCode, cursorCurrent, terminalExpression);   

                % ONCE THE CODE REACHES END OF EXPRESSION, AND IT'S NOT WITHIN A SCOPE BEYOND ROOT
                if endOfExpression && codeDepth == 0         
                    % SELECT AND CLEAN THE DETECTED EXPRESSION
                    rawExpression = strCode (cursorFirst:cursorCurrent);
                    rawExpression = Language.cleanCode (rawExpression);
                    Debug.LogLine (['End of Expression. Code selected is <' rawExpression '> with codeType <' ...
                        num2str(codeType) '>']);
                    Debug.LogLine (['---------------------------------------']);


                    % IF THE CODE WASN'T A COMMENT AND WASN'T LINKER CODE
                    if codeType ~= EnumCode.CODE_COMMENTS() && codeType ~= EnumCode.CODE_LINKER_1()
                        % A PIECE OF CODE WAS INTERPRETED
                        nthInterpretedCode = nthInterpretedCode + 1;

                        % OBTAIN EXPRESSION TYPE
                        % DO PREPROCESSING FOR TOKENISER
                        expType = Language.processExpressionByLexeme  (rawExpression, codeType);

                        % ADD TO strCode OBJECT
                        strCodeExpression = SpaghettiExpression;
                        strCodeExpression.SetCode(rawExpression, expType);
                        if codeType ~= 0
                            strCodeExpression.HasScope = 1;
                        else 
                            strCodeExpression.HasScope = 0;
                        end

                        Debug.LogLine (['=======================================']);

                        Debug.LogLine (['Expression <' rawExpression '> is appended at index <' ...
                            num2str(nthInterpretedCode) '>']);

                        % APPEND TO INTERPRETED CODE
                        interpretedCode(nthInterpretedCode) = strCodeExpression;
                    
                    % IF THE CODE WAS LINKER CODE
                    elseif codeType == EnumCode.CODE_LINKER_1()
                        % A PIECE OF CODE WAS INTERPRETED
                        nthInterpretedCode = nthInterpretedCode + 1;

                        % SELECT THE ENTIRE EXPRESSION
                        rawExpression = strCode (cursorFirst:cursorCurrent);
                        rawExpression = Language.cleanCode (rawExpression);

                        % ADD LINKER CODE
                        strCodeExpression = SpaghettiExpression;
                        strCodeExpression.CodeType = codeType;
                        strCodeExpression.SetCode(rawExpression, EnumExpression.EXP_LINKER());
                        
                        Debug.LogLine (['=======================================']);

                        Debug.LogLine (['Expression <' rawExpression '> is appended at index <' ...
                            num2str(nthInterpretedCode) '>']);

                        % APPEND TO INTERPRETED CODE
                        interpretedCode(nthInterpretedCode) = strCodeExpression;
                    end
                    
                    Debug.LogLine (['---------------------------------------']);
                    Debug.LogLine (['Expression array is now <' mat2str(size (interpretedCode)) '>']);

                    cursorFirst = cursorCurrent + 1; % MOVE TO NEXT INSTRUCTION
                    
                    % RESET
                    endOfExpression = false; 
                    codeType = EnumCode.CODE_GENERIC();
                end
            end

            Debug.LogLine (['Returning expression array of <' mat2str(size (interpretedCode)) '>']);
            o = interpretedCode;
            Debug.LogLine (['---------------------------------------']);
            Debug.LogLine (['Returned expression array of <' mat2str(size (o)) '>']);
            Debug.LogLine (['=======================================']);
        end
    end
end