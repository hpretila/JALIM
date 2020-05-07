% CONTAINER FOR UNITS OF CODE PREPROCESSED PRIOR TO BEING INTERPRETED; USED FOR 
% INITIAL CODE INTERPRETATION AND FOR EVALUATING SCOPES
classdef SpaghettiLexeme < handle
    properties
        % TYPES OF LEXEMES
        % 0 - IDENTIFIER (float, int, double, char, string, void)
        % 1 - ASSIGNMENT (=)
        % 2 - OPERATOR (==, =<, =>, >, <, +, -, *, /)
        % 3 - ARGUMENT SCOPE (if(expression), voidname(expression))
        % 4 - NESTED SCOPE ((expression))
        % 5 - SCOPE ({expression})
        % 6 - DIGITS (1,2,3,...)
        % 7 - STRING LITERALS ("woah")
        % 8 - VARNAMS
        Type = 8

        % ACTUAL DATA
        Data

        % ERRONNOUS
        HasError = 0 % FALSE
    end

    % METHODS PER OBJECT
    methods
        % SET VALUES FOR SpaghettiLexeme OBJECT
        function obj = SetData (obj, inData, inType, inHasError)
            obj.Type = inType;
            obj.Data = inData;
            obj.HasError = inHasError;
        end
    end

    % GENERAL METHODS FOR LEXEMES
    methods (Static)
        
        % EXPRESSION GUIDE
        % 0 - EXPRESSION EXECUTION Where code is run, to return a value if not void
        % 1 - LHSRHS INITIALISATION Where assignment and object creation is done
        % 1 - 2 - LHSRHS ASSIGNMENT Where assignment is done to an object (LHS) and a scope (RHS)
        % 1 - 3 - LHSRHS (RETURN) COMPARISON Where an operator is used on two scopes (LHS,RHS) to return a 0,1 value
        % 4 - SCOPED CODE var funcname () {code}
        % 5 - CONDITIONAL SCOPED CODE if (bool), void (code)

        % CODE TYPE GUIDE
        % 0 = NORMAL, DIVIDE BY ';'
        % 1 = FUNCTION ARGUMENT, DIVIDE BY ')' AND WILL SKIP ARGUMENTS
        % 2 = SCOPED CODE, DIVIDE BY '}' AND WILL SKIP SCOPED CODE
        % 3 = COMMENTS, DIVIDE BY '\n' INITIATED BY '//'
        % 4 = LINKERCODE1, DIVIDE BY '\n' INITIATED BY '#include'
        % 5 = (UNIMPLEMENTED) LINKERCODE2, DIVIDE BY '#enddef' INITIATED BY '#ifdef'

        % RECEIVES AN EXPRESSION (SpaghettiExpression type),  AND
        % RETURNS AN ARRAY OF LEXEMES (SpaghettiLexeme type)
        function o = parseExpression (expression, expressionType)
            Debug.LogLine (['Expression type ' mat2str(expressionType)]);
            
            try
                % GET RID OF THE SEMICOLON
                if expression(length(expression)) == ';'
                    expression(length(expression)) = '';
                end
            catch
            end
            
            % TYPE CAST TO CHAR.
            expression = [char(expression) char(187)];


            % INITIALISE ARRAY
            lexemes(1) = SpaghettiLexeme;

            % READING VARIABLES
            nthLexeme = 1;
            
            % SIFT THROUGH EACH CHARACTER AND IDENTIFY LEXEMES
            initialCursor = 1;
            readingLexeme = false;
            readDepth = 0;

            % DEFAULT VALUES FOR LEXEMES
            lexemeData = '';
            
            % READING VALUES
            delimiter = char(187);
            switch expressionType
                % FOR ARGUMENTS, SPLIT LEXEMES VIA COMMAS
                case EnumExpression.EXP_ARGUMENTS()
                    delimiter = ',';
                otherwise
                    delimiter = char(187);
            end
            terminatingBracket = '©©©©©©©©©©©©©©©©©©©©©©©©©©';
            currentTermBracket = '©';
            readType = 8; % DEFAULT TO REFERENCE
            hasError = 0; % DEFAULT TO NO ERRORS
            
            for currentCursor = 1:length(expression)
                % GRAB CURRENT VALUE AT CURSOR
                cursorVal = expression(currentCursor);

                % IF IT'S THE END, SELECT THE DATA AND SET IT
                if cursorVal == delimiter && readDepth == 0 && readingLexeme
                    Debug.SpecialLogLine (['Saving the expression <' lexemeData '> at [' num2str(currentCursor) '] value <' cursorVal '> type <' num2str(readType) '>']);
                    Debug.LogLine ("=======================================");
                    
                    % STOP READING
                    readingLexeme = false;

                    % SET TYPE AND DATA
                    lexeme.SetData (lexemeData, readType, hasError);
                    lexemes (nthLexeme) = lexeme;
                    
                    % MOVE TO THE NEXT LEXEME
                    nthLexeme = nthLexeme + 1;
                    readingLexeme = false;
                end
                
                % CURENT TERMINATING BRACKET VALUE
                if readDepth+1 <= length(terminatingBracket) && readDepth ~= 0 
                    currentTermBracket = terminatingBracket(readDepth+1);
                else
                    Debug.LogLine (['Attempted to read index [' num2str(readDepth+1) '] from an array of [' num2str(length(terminatingBracket)) ']'])
                end
                
                % PROCESS CURRENT LEXEME
                if initialCursor ~= currentCursor
                    currentLexeme = expression((initialCursor):(currentCursor));
                % IF THE CURSOR IS ON THE INITIAL CURSOR, THE EXPRESSION IS ON THE CURRENT CURSOR
                else
                    currentLexeme = expression (currentCursor);
                end
                Debug.LogLine (['The current depth is <' num2str(readDepth) '> and current lexeme is <' lexemeData '> while the cursor value is ' cursorVal]);
                Debug.LogLine (['Also, the terminating string collection is <' terminatingBracket '> with <' currentTermBracket '> as a terminating bracket.']);

                
                % SCOPE IS INITIATED
                if Language.isScopeInitiator (cursorVal)
                    Debug.LogLine ("A SCOPE WAS DETECTED.");
                    % INCREMENT readDepth
                    terminatingBracket(readDepth+1) = Language.terminatorFromInitiator (cursorVal);
                    readDepth = readDepth + 1;
                % SCOPE IS TERMINATED
                elseif readDepth <= length(terminatingBracket) && readDepth ~= 0 
                    if strcmp (cursorVal, terminatingBracket(readDepth))
                        Debug.LogLine ("TERMINATION.");
                        % DELETE RECORD
                        terminatingBracket(readDepth+1) = char(174);

                        % DECREMENT readDepth
                        readDepth = readDepth - 1;
                    end

                % STRING LITERAL IS INITIATED
                elseif cursorVal == '"'
                    % INCREMENT readDepth
                    terminatingBracket(readDepth+1) = '"';
                    readDepth = readDepth + 1;
                end

                % UPDATE CURRENT TERM BRACKET
                currentTermBracket = terminatingBracket (readDepth+1);

                % INITIATE LEXEME OBJECT
                lexeme = SpaghettiLexeme;

                % IF THE CODE IS SET UP TO READ A LEXEME AND IF THERES NO SCOPE AND THERE IS A SPACE
                if readingLexeme && readDepth == 0
                    
                    Debug.LogLine (cursorVal);
                    
                    % DEFAULT VALUE
                    lexemeData = currentLexeme;

                    % IF IT'S THE TERMINATING BRACKET
                    if strcmp (cursorVal, currentTermBracket)
                        Debug.SpecialLogLine ('Now collecting for scopes.');

                        % IF THE CODE IS NOT A STRING LITERAL
                        if Language.isScopeInitiator (currentLexeme(1))
                            % IF THE LEXEME BEFORE IT IS A VARNAME, IT IS AN ARGUMENT SCOPE
                            if lexemes(nthLexeme-1).Type == EnumLexeme.LEX_VARS() 
                                lexemeData = currentLexeme (2:length(currentLexeme)-1);
                                readType = EnumLexeme.LEX_SCOPEARGS(); % TREAT AS ARGUMENT SCOPE
                            
                            % IF THE LEXEME BEFORE IT IS AN ARGUMENT SCOPE, IT IS THE BODY SCOPE
                            elseif lexemes(nthLexeme-1).Type == EnumLexeme.LEX_SCOPEARGS()
                                % TREAT AS SCOPE
                                lexemeData = currentLexeme (2:length(currentLexeme)-1);
                                readType = EnumLexeme.LEX_SCOPE(); % TREAT AS SCOPED CODE
                            % IF THE SCOPE IS NEITHER ARGUMENT OR BODY SCOPE, IT IS NESTED SCOPE
                            else
                                if cursorVal == "}"
                                    disp ("Warning: Code contains an unheaded scope. Consider using ().");
                                end
                                % IGNORE BRACKETS AND TRAILING SPACE
                                lexemeData = currentLexeme (2:length(currentLexeme)-1);
                                readType = EnumLexeme.LEX_SCOPENESTED(); % TREAT AS NESTED CODE
                            end
                        else
                            % TREAT AS SCOPE IF IT'S A STRING LITERAL
                            lexemeData = currentLexeme (2:length(currentLexeme)-1);
                            readType = EnumLexeme.LEX_STRINGS(); % TREAT AS STRING LITERAL
                        end

                        Debug.LogLine (['The new highlighted lexeme is: ' lexemeData ' when ' (currentLexeme (2:length(currentLexeme)-1)) ' is expected']);

                    % IF IT'S NOT A TERMINATING BRACKET
                    else
                        Debug.LogLine (['Trying to classify currentLexeme <' currentLexeme '>'])

                        % SET LEXEME TYPE
                        readType = Language.classifyLexeme (currentLexeme);
                    end

                % IF THE CODE IS NOT SET UP TO READ
                else
                    % ONCE THE LEXEME IS NO LONGER A SPACE AND IT IS NOT YET READING, SET THE PROGRAM TO READ
                    if cursorVal ~= delimiter && ~readingLexeme
                        readingLexeme = true;

                        % RESET THE INITIAL CURSOR, CURRENT LEXEME, TYPE AND LEXEME DATA
                        initialCursor = currentCursor;
                        currentLexeme = expression (currentCursor);
                        lexemeData = expression (currentCursor);
                        readType = Language.classifyLexeme (currentLexeme);

                        Debug.LogLine(['Initial cursor position set to ' num2str(initialCursor)])
                    end
                end
                Debug.LogLine ("---------------------------------------");
            end

            % RETURN LEXEMES
            o = lexemes;
        end
    end
end