% SPAGHETTI RUNTIME DOESN'T COMPILE THE CODE INTO INTERMIDIARY
% CODE, AND INSTEAD RUNS IT AS IT IS PARSED. STRUCTURES ARE
% HARDCODED HERE, BUT IN A FUTURE REVISION, A MORE MALLEABLE
% APPROACH WOULD BE MORE PREFERRABLE
classdef SpaghettiRuntime < handle
    properties
        memoryTable = SpaghettiVariable; % ARRAY OF SpaghettiVariable
        expressionLog = SpaghettiExpression; % ARRAY OF SPAGHETTI EXPRESSION
        workingDir
    end
    methods
        % ======================================================================
        % MAIN RUNTIME

        % INITIALISE RUNTIME
        function obj = Init (obj)
            % SETTING UP RUNTIME
            memTable_i = 1;

            % INITIALISE MEMORY TABLE WITH RETURN VARIABLE
            returnVariable = SpaghettiVariable;
            returnVariable.Name = "return";
            returnVariable.Value = '';
            returnVariable.Type = EnumVariable.CHAR();
            obj.memoryTable(memTable_i) = returnVariable;

            % THEN INITIALISE ALL RESERVED LEXEMES
            for reservedLexeme = SpaghettiStandardLibrary.reservedFunctions()
                % INCREMENT INDEX
                memTable_i = memTable_i + 1;
                
                % ADD RESERVED FUNCTION NAME
                reservedVariable = SpaghettiVariable;
                reservedVariable.Name = char(cellstr(reservedLexeme));
                reservedVariable.Value = '';
                reservedVariable.Type = EnumVariable.CHAR();
                reservedVariable.isFunc = true;
                obj.memoryTable(memTable_i) = reservedVariable;
            end
        end

        function obj = ParseFile (obj, filename)
            % IMPORT STRING FROM FILE
            file = importdata(filename);

            % PARSE FILE
            obj.Parse(file);
        end

        % INTERPRET CODE
        function obj = Parse (obj, code)
            % FIRST GRAB ALL THE EXPRESSIONS FROM THE CODE
            code_Expressions = Language.spaghettiGetExpression(...
                StringHelper.cell2CodeStr(code));
            % CHECK EACH EXPRESSION ONE BY ONE
            for i = 1:length(code_Expressions)
                % GET EXPRESSION
                expression = code_Expressions (i);

                % SAVE EXPRESSION
                obj.expressionLog(length(obj.expressionLog)+1) = expression;

                if expression.CodeType == EnumExpression.EXP_LINKER()
                    obj.ParseFile([expression.RawCode(11:(length(expression.RawCode)-1))]);
                % IF IT'S NOT COMPILER CODE
                else
                    obj.Run (expression);
                end
            end
        end

        % ======================================================================
        % RUNNING CODE

        % MASTER RUN FUNCTION; WILL DECIDE WHICH METHOD TO USE TO PARSE THE EXPRESSION
        function obj = Run (obj, code)
            SpaghettiRuntime.RunReturnable (obj, code);
        end

        
        % ======================================================================
        % OBJECT ASSIGMENT

        % RUN VARIABLE AND FUNCTION ASSIGNMENT
        function obj = RunAssignmentFunction (obj, code)
            Debug.SpecialLogLine ("---------------------------------------");
            Debug.SpecialLogLine ("Running Function Assignment.");

            % OBTAIN ALL THE CODE
            a_code = code.Code;

            % SET IS RETURN VALUES
            isReturn = false;
            
            % CREATE A VARIABLE BASED ON THE ASSIGNMENT
            variable = SpaghettiVariable;

            % SET VARIABLE TYPES.
            variable.Type = a_code(1).Data;
            variable.Name = a_code(2).Data;
            variable.Arguments = a_code(3).Data;
            variable.Code = a_code(4).Data;
            variable.isFunc = true;

            % DEFINE THE NTH_VALUE
            nth_value = length(obj.memoryTable) + 1;

            % IF IT ALREADY EXISTS, CHANGE PREEXISTING VALUE.
            if SpaghettiRuntime.IsNameExist (obj, variable.Name)
                % CHECK IF THE VARIABLE ISN'T A FUNCTION
                Debug.Throw ("SyntaxError: Tried assigning a function to a existing object.")
            end

            % ADD TO MEMORY TABLE IF NOT INVALID
            if ~strcmp(variable.Type, EnumVariable.INVALID())
                obj.memoryTable (nth_value) = variable;
                Debug.SpecialLogLine (['Setting variable <' variable.Name '> with code <' variable.Code '>']);
            else
                Debug.LogLine ("Read null variable.");
            end
        end

        function obj = RunAssignment (obj, code)
            Debug.SpecialLogLine ("---------------------------------------");
            Debug.SpecialLogLine ("Running Assignment.");

            % SET IS RETURN VALUES
            isReturn = false;
            
            % CREATE A VARIABLE BASED ON THE ASSIGNMENT
            variable = SpaghettiVariable;

            % DEFINE THE NTH_VALUE
            nth_value = length(obj.memoryTable) + 1;

            % OBTAIN CODE FROM EXPRESSION
            if strcmp (code.Code(1).Data, 'return')
                Debug.LogLine ("Return detected, return VAL");

                % IF IT'S A RETURN TYPE, THERE IS NO NAME SETTING, JUST IGNORE 'return'.
                a_code = SpaghettiRuntime.UnfoldGenericExpression (obj, code, 1);

                Debug.SpecialLogLine ("---------------------------------------");
                Debug.SpecialLogLine ("SETTING VAR TYPES.");
                
                % SET VARIABLE TYPES.
                variable.Name = 'return';
                variable.Type = SpaghettiRuntime.FindVariableType (obj, a_code(2));
                variable.Value = a_code(2).Data;

                % RETURN IS RESERVED FOR FIRST INDEX
                %nth_value = 1;
                
                Debug.SpecialLogLine ("---------------------------------------");
                Debug.SpecialLogLine (['THE RESULT IS return = ' a_code(2).Data]);

            elseif code.Code(1).Type == EnumLexeme.LEX_IDENT()
                Debug.LogLine ("No return detected, IDENT VAR ASSIGN VAL");
                isReturn = false;

                % IF IT ISN'T A RETURN VAL ASSIGNMENT, IGNORE THE NAME AT INDEX 2
                a_code = SpaghettiRuntime.UnfoldGenericExpression (obj, code, 2);

                % SET VARIABLE TYPES.
                variable.Type = a_code(1).Data;
                variable.Name = a_code(2).Data;
                variable.Value = a_code(4).Data;
                %Debug.SpecialLogLine ([variable.Type char(187) a_code(2).Data '=' variable.Value])
            else
                Debug.LogLine ("No return detected, VAR ASSIGN VAL");
                isReturn = false;

                % IF IT ISN'T A RETURN VAL ASSIGNMENT, IGNORE THE NAME AT INDEX 1
                a_code = SpaghettiRuntime.UnfoldGenericExpression (obj, code, 1);
                var_orig = SpaghettiRuntime.GetVariable(obj, a_code(1).Data);

                % SET VARIABLE VALUE.
                variable.Type = var_orig.Type;
                variable.Name = var_orig.Name;
                
                % RUN THE VALUE TO BE DOUBLY SURE.
                variable.Value = a_code(3).Data;
                lexExp = Language.spaghettiGetExpression(...
                    StringHelper.cell2CodeStr(variable.Value));
                lex = SpaghettiRuntime.RunReturnable (obj, lexExp);
                variable.Value = lex.Data;
                %Debug.SpecialLogLine ([variable.Type char(187) a_code(2).Data '=' variable.Value])
            end

            % THE MEMORY TABLE IS MADE OF STRINGS :P
            if isnumeric (variable.Value)
                variable.Value = num2str (variable.Value);
            end

            % IF IT ALREADY EXISTS, CHANGE PREEXISTING VALUE.
            if SpaghettiRuntime.IsNameExist (obj, variable.Name)
                nth_value = SpaghettiRuntime.FindVariableIndex (obj, variable.Name);
                Debug.LogLine (strcat("Setting preexisting variable at ", num2str(nth_value) ));
            end

            obj.memoryTable (nth_value) = variable;
            Debug.SpecialLogLine (['Setting variable <' variable.Name '> with data <' variable.Value '>']);

        end 

        % ======================================================================
        % LOGGING/CLI OUTPUT

    end
    methods (Static)
        % ======================================================================
        % RUNNING CODE

        % RUN GENERIC CODE; ENTRY POINT FOR ALL INSTRUCTIONS
        function RunGeneric (obj, code)
            expr = UnfoldGenericExpression (obj, code, 0);
        end

        % UNFOLD EXPRESSIONS, TO ENSURE THAT ALL VALUES HAVE BEEN PROCESSED
        % RETURNS LEXEMES PROCESSED FROM AN EXPRESSION
        function o = UnfoldGenericExpression (obj, code, assignIndex)
            
            Debug.SpecialLogLine ("---------------------------------------");
            Debug.SpecialLogLine (strcat("Unfolding ", code.RawCode));

            % INITIALISE THE OUTPUT ARRAY
            outLex (1) = SpaghettiLexeme;

            % WORK THROUGH EACH LEXEME IN THE EXPRESSION
            i = 1;
            while i <= length(code.Code)
                lex = code.Code(i);
                Debug.SpecialLogLine ("***************************************");
                Debug.SpecialLogLine (strcat("Working on ", lex.Data, " [", num2str(i), "]"));
                % IF THERE'S NESTED CODE
                if lex.Type == EnumLexeme.LEX_SCOPENESTED()
                    % RETURN LEXEME DATA AS EXPRESSIONS
                    lexExp = Language.spaghettiGetExpression(...
                        StringHelper.cell2CodeStr(lex.Data));
                    lex = SpaghettiRuntime.RunReturnable (obj, lexExp);
                % IF THE CURRENT LEXEME IS A VARIABLE NAME
                elseif lex.Type == EnumLexeme.LEX_VARS()
                    % RETURNS VARIABLE DATA, IF IT ISN'T A ASSIGNMENT.
                    % ASSIGNMENTINDEX CAN BE A NON-VALUE.
                    if i ~= assignIndex && ~strcmp(code.Code, '')
                        lex = SpaghettiRuntime.RunReturnableLexeme(obj, i, code.Code);
                    end
                end
                % DEBUG
                Debug.SpecialLogLine (".......................................");
                Debug.SpecialLogLine (strcat("Finalised lexeme for <", code.RawCode, "> is ", lex.Data))

                % ADDS THE LEXEME TO THE OUTPUT ARRAY
                outLex (i) = lex;

                % INCREMENTS ITERATOR
                i = i + 1;
            end

            % RETURNS THE OUTPUT ARRAY
            o = outLex;
        end

        % RUN RETURNALE CODE
        function lex = RunReturnable (obj, code)
            Debug.SpecialLogLine ("=======================================");
            Debug.SpecialLogLine (strcat("Running body code <", code(1).RawCode, ">"));

            % ONLY CONSIDERS ONE EXPRESSION AT A TIME.
            code = code(1);
            
            % DEFINE EXPRESSION
            lexCode = code.Code;

            % DEFINE LEXEME
            lex = SpaghettiLexeme;
            lex.Data = '';

            % IF THERE IS CODE THIS WILL RUN
            if (~strcmp(code.RawCode,'') && 0 < length(char(cellstr(code.RawCode))))
                o = code.RawCode (1);
                % ----------------------------------------------------------------------
                % FIND CODE PATTERNS AND RUN ASSOCIATED CODE
                % THERE IS A LIKELIHOOD THAT THERE MIGHT NOT BE ENOUGH LEXEMES AND WILL THROW AN EXCEPTION

                % COND1: IDENTIFIER VARS ASSIGN VARS/STRINGS/DIGITS
                try
                    cond_1 = lexCode(1).Type == EnumLexeme.LEX_IDENT() && ...
                    lexCode(2).Type == EnumLexeme.LEX_VARS() && ...
                    lexCode(3).Type == EnumLexeme.LEX_ASSIGN();
                catch
                    cond_1 = false;
                end

                % COND2: VARS ASSIGN VARS/STRINGS/DIGITS
                try
                    cond_2 = lexCode(1).Type == EnumLexeme.LEX_VARS() && ...
                    lexCode(2).Type == EnumLexeme.LEX_ASSIGN();
                catch
                    cond_2 = false;
                end
                
                % COND3: IDENTIFIER VARS ARG SCOPE
                try
                    cond_3 = lexCode(1).Type == EnumLexeme.LEX_IDENT() && ...
                    lexCode(2).Type == EnumLexeme.LEX_VARS() && ...
                    lexCode(3).Type == EnumLexeme.LEX_SCOPEARGS() && ...
                    lexCode(4).Type == EnumLexeme.LEX_SCOPE();
                catch
                    cond_3 = false;
                end

                % COND4: VARS ARG
                try
                    cond_4 = lexCode(1).Type == EnumLexeme.LEX_VARS() && ...
                    lexCode(2).Type == EnumLexeme.LEX_SCOPEARGS();
                catch
                    cond_4 = false;
                end

                % COND5: return VARS/STRINGS/DIGITS
                try
                    cond_5 = strcmp(lexCode(1).Data, 'return') && 1 < length (lexCode);
                catch
                    cond_5 = false;
                end

                % COND6: VARS/STRINGS/DIGITS OPERATOR VARS/STRINGS/DIGITS
                try
                    cond_6 = lexCode(2).Type == EnumLexeme.LEX_OPERATOR;
                catch
                    cond_6 = false;
                end

                % COND7: VARS
                try
                    cond_7 = length(lexCode) == 1 && ...
                    0 < length(lexCode(1).Data) && ...
                    lexCode(1).Type == EnumLexeme.LEX_VARS;
                catch
                    cond_7 = false;
                end
                
                % ----------------------------------------------------------------------
                % GO THROUGH EACH CONDITION
                    
                % IDENTIFIER VARS ASSIGN VARS/STRINGS/DIGITS
                if cond_1
                    Debug.SpecialLogLine ('IDENTIFIER VARS ASSIGN VARS/STRINGS/DIGITS')
                    obj.RunAssignment (code);

                % VARS ASSIGN VARS/STRINGS/DIGITS
                elseif cond_2
                    Debug.SpecialLogLine ('VARS ASSIGN VARS/STRINGS/DIGITS')
                    obj.RunAssignment (code);
                
                % IDENTIFIER VARS ARG SCOPE
                elseif cond_3
                    Debug.SpecialLogLine ('IDENTIFIER VARS ARG SCOPE')
                    obj.RunAssignmentFunction (code);
                    
                % return VARS/STRINGS/DIGITS
                elseif cond_5
                    Debug.SpecialLogLine ('return VARS/STRINGS/DIGITS')
                    obj.RunAssignment (code);
                
                % VARS ARG
                elseif cond_4
                    Debug.SpecialLogLine ('VARS ARG')
                    if (~strcmp(code.RawCode,''))
                        lex = SpaghettiRuntime.RunReturnableLexeme (obj, 1, lexCode);
                    end

                % VARS OPERATOR VARS
                elseif cond_6
                    Debug.SpecialLogLine ('VARS OPERATOR VARS')
                    lex = SpaghettiRuntime.RunOperation (obj, code);

                % VARS
                elseif cond_7
                    Debug.SpecialLogLine ('VARS')
                    % GRAB ANY VARIABLE WITH THIS NAME
                    lex = SpaghettiRuntime.RunReturnableLexeme (obj, 1, lexCode);

                % IF IT'S ONLY ONE UNRECOGNISED TOKEN, IT'S PROBABLY A VARIABLE
                elseif length(lexCode) == 1
                    lex = lexCode(1);
                else
                    disp ("Warning: Unrecognised pattern; the code will not try to execute it.")
                end                
            % OTHERWISE, GO BONKERS
            else
                Debug.SpecialLogLine ('EMPTY EXPRESSION');
            end
        end

        % RUN OPERATIONS
        function o = RunOperation (obj, expression)
            Debug.SpecialLogLine ("---------------------------------------");
            Debug.SpecialLogLine ("Running Operation.");
            Debug.SpecialLogLine (".......................................");

            % OBTAIN CODE FROM EXPRESSIONS
            a_code = SpaghettiRuntime.UnfoldGenericExpression (obj, expression(1), 0);

            % INSTANTIATE LEXEME FOR RETURN VALUE
            lex = SpaghettiLexeme;
            lex.Type = EnumLexeme.LEX_BOOL();

            % OPERATOR IS ALWAYS SECOND LEXEME OF EXPRESSION
            operator = a_code (2).Data;
            
            % EQUATION
            if strcmp (operator, '==')
                % COMPARE BOTH OPERANDS
                if strcmp (a_code (1).Data, a_code (3).Data)
                    lex.Data = 'true';
                else
                    lex.Data = 'false';
                end
            
            % NEQ
            elseif strcmp (operator, '!=')
                % COMPARE BOTH OPERANDS
                if strcmp (a_code (1).Data, a_code (3).Data)
                    lex.Data = 'false';
                else
                    lex.Data = 'true';
                end

            % LESS THAN
            elseif strcmp (operator, '<')
                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % COMPARE BOTH OPERANDS
                if a_data < b_data
                    lex.Data = 'true';
                else
                    lex.Data = 'false';
                end

            % GREATER THAN
            elseif strcmp (operator, '>')
                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % COMPARE BOTH OPERANDS
                if a_data > b_data
                    lex.Data = 'true';
                else
                    lex.Data = 'false';
                end

            % LESS THAN OR EQUAL TO
            elseif strcmp (operator, '<=')
                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % COMPARE BOTH OPERANDS
                if a_data <= b_data
                    lex.Data = 'true';
                else
                    lex.Data = 'false';
                end

            % GREATER THAN OR EQUAL TO
            elseif strcmp (operator, '>=')
                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % COMPARE BOTH OPERANDS
                if a_data >= b_data
                    lex.Data = 'true';
                else
                    lex.Data = 'false';
                end

            % ------------------------------------------
            % BASIC MATHEMATIC OPERATIONS
            
            % ADDITION
            elseif strcmp (operator, '+')
                % RETURN TYPE IS A DIGIT
                lex.Type = EnumLexeme.LEX_DIGITS();

                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % CALCULATE
                lex.Data = num2str (a_data + b_data);
            
            % SUBTRACTION
            elseif strcmp (operator, '-')
                % RETURN TYPE IS A DIGIT
                lex.Type = EnumLexeme.LEX_DIGITS();

                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % CALCULATE
                lex.Data = num2str (a_data - b_data);

            % ADDITION
            elseif strcmp (operator, '*')
                % RETURN TYPE IS A DIGIT
                lex.Type = EnumLexeme.LEX_DIGITS();

                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % CALCULATE
                lex.Data = num2str (a_data * b_data);

            % ADDITION
            elseif strcmp (operator, '/')
                % RETURN TYPE IS A DIGIT
                lex.Type = EnumLexeme.LEX_DIGITS();

                % CONVERT OPERANDS INTO NUMERICAL VALUES
                a_data = str2double (a_code (1).Data);
                b_data = str2double (a_code (3).Data);

                % CALCULATE
                lex.Data = num2str (a_data / b_data);
            end

            % OUTPUT LEXEME
            o = lex;
        end

        % WHEN THE LEXEME IS A REFERENCE
        function lex = RunReturnableLexeme (obj, index, lexArray)
            Debug.SpecialLogLine ("---------------------------------------");
            Debug.SpecialLogLine ("Getting Returnable Lexeme.");
            Debug.SpecialLogLine (".......................................");

            % FIND THE CURRENT LEXEME FROM THE SUPERARRAY
            varName = lexArray (index).Data;

            % MAKE SURE IT'S NOT EMPTY
            if (~strcmp(varName,'') && 0 < length(char(varName)))
                Debug.SpecialLogLine (strcat("Finding variable ", varName));

                % FIND ELEMENT ON MEMORY TABLE THAT CONTAINS NAME
                variable = SpaghettiRuntime.GetVariable (obj, varName);

                % IF IT ISN'T A FUNCTION
                if ~variable.isFunc
                    Debug.SpecialLogLine ("Found variable.");
                    lexeme = SpaghettiLexeme;

                    % THE LEXEME CONTAINS VARIABLE VALUE
                    lexeme.Data = variable.Value;
                    Debug.SpecialLogLine (strcat("Variable has value ", variable.Value, " and type ", variable.Type));
                    
                    % RETURN A LEXEME EXPRESSING VARIABLE
                    if strcmp(variable.Type, EnumVariable.INT()) || strcmp(variable.Type, EnumVariable.DOUBLE())
                        lexeme.Type = EnumLexeme.LEX_DIGITS ();
                    elseif strcmp(variable.Type, EnumVariable.STRING()) || strcmp(variable.Type, EnumVariable.CHAR())
                        lexeme.Type = EnumLexeme.LEX_STRINGS ();
                    elseif strcmp(variable.Type, EnumVariable.BOOL())
                        lexeme.Type = EnumLexeme.LEX_BOOL ();
                    end
                    lex = lexeme;
                    
                %IF IT IS
                else
                    Debug.SpecialLogLine ("Found Function.");

                    % GET ARGUMENTS, WHICH ARE AFTER THE VAR_NAME
                    args = lexArray (index + 1).Data;

                    % NOT ALL FUNCTIONS HAVE SCOPES
                    try
                        % GET SCOPE, WHICH IS AFTER THE ARGUMENTS
                        scope = lexArray (index + 2).Data;
                    catch
                        scope = '';
                    end

                    % RUN THE RUNRETURNABLEFUNCTION ROUTINE
                    lex = SpaghettiRuntime.RunReturnableFunction (obj, variable, args, scope);
                end
            else
                lex = lexArray (index);
            end

            % DEBUG
            Debug.SpecialLogLine (".......................................");
            returnedName = lex.Data;
            Debug.SpecialLogLine (strcat("Returned lexeme ", returnedName)); 
        end

        % RUN FUNCTION TO RETURN A LEXEME
        function lex = RunReturnableFunction (obj, variable, args, scope)
            Debug.SpecialLogLine ("---------------------------------------");

            % IF THE CODE NAME IS RESERVED FOR STANDARD PATTERNS LIKE CONDITIONALS
            if (SpaghettiStandardLibrary.isReservedLexeme (variable.Name))
                Debug.SpecialLogLine (strcat("Running Reserved Function,", variable.Name));
                % SEND THE VARIABLES OFF TO A FUNCTION
                SpaghettiStandardLibrary.RunReservedFunction (obj, variable, args, scope);
            else
                Debug.SpecialLogLine (strcat("Running Function,", variable.Name));
                % THE EVENTUAL CODE TO BE RUN
                final_code = '';
                
                % IF THE DEFINED ARGUMENTS AREN'T EMPTY
                if 0 < length(variable.Arguments)
                    % FIRST INSTANTIATE THE ARGUMENTS
                    defargs = strsplit(variable.Arguments, ', ');
                    inargs = strsplit(args, ', ');

                    % IF THESE TWO MATCH, CONTINUE. OTHERWISE, SOMETHING'S WRONG.
                    if length(defargs) ~= length (inargs)
                        Debug.Throw (['SyntaxError: Tried calling <' variable.Name '> but argument sizes mismatched.']);
                    end
                    
                    % PREPEND THESE ARGUMENTS AS VARIABLE DECLARATIONS
                    for i = 1:length(defargs)
                        final_code = strcat(final_code, (defargs(i)), ' = ', (inargs(i)), ';');
                    end
                end

                % APPEND THE SCOPED CODE
                final_code = strcat(final_code, variable.Code);

                % FINALLY, RUN IT ALL.
                obj.Parse (final_code);
            end

            % GET RETURN VALUE
            lex = SpaghettiRuntime.RunReturn (obj); 
        end

        % GETS RETURN VALUE
        function lexeme = RunReturn (obj)
            % GET RETURN VARIABLE
            variable = SpaghettiRuntime.GetVariable (obj, "return");
            
            % GENERATE LEXEME
            lexeme = SpaghettiLexeme;
            lexeme.Data = variable.Value;
            
            % RETURN A LEXEME TYPE EXPRESSING VARIABLE
            if strcmp(variable.Type, EnumVariable.INT()) || strcmp(variable.Type, EnumVariable.DOUBLE())
                lexeme.Type = EnumLexeme.LEX_DIGITS ();
            elseif strcmp(variable.Type, EnumVariable.STRING()) || strcmp(variable.Type, EnumVariable.CHAR())
                lexeme.Type = EnumLexeme.LEX_STRINGS ();
            elseif strcmp(variable.Type, EnumVariable.BOOL())
                lexeme.Type = EnumLexeme.LEX_BOOL ();
            end
        end

        % ======================================================================
        % VARIABLE OPERATIONS

        % FIND VARIABLE TYPE, IF VARIABLE ALREADY EXISTS OR IS A STRING LITERAL/DIGIT
        function vartype = FindVariableType (obj, lex)
            switch lex.Type
                % IF IT IS A BOOL, DEFAULT TO BOOL
                case EnumLexeme.LEX_BOOL ()
                    varType = EnumVariable.BOOL();                
                % IF IT IS A VARIABLE NAME, IT SHOULD HAVE EXISTED BEFORE
                case EnumLexeme.LEX_VARS ()
                    % GRAB EXISTING VARIABLE
                    Debug.LogLine(['Try to find ' lex.Data]);
                    variable = SpaghettiRuntime.GetVariable (obj, lex.Data);

                    % RETURN EXISTING VARIABLE'S TYPE
                    vartype = variable.Type;
                % IF IT IS A SET OF DIGITS, DEFAULT TO INTEGER
                case EnumLexeme.LEX_DIGITS ()
                    vartype = EnumVariable.INT();
                % IF IT IS A STRING LITERAL, RETURN A STRING
                case EnumLexeme.LEX_STRINGS ()
                    vartype = EnumVariable.STRING();
                % OTHERWISE, IT'S PROBABLY NOT A VARIABLE
                otherwise
                    vartype = EnumVariable.INVALID();
            end
        end

        % FINDS THE VARIABLE AND THROWS AN ERROR IF IT CAN'T FIND IT
        function var = GetVariable (obj, name)
            % GET THE EXPRESSIONS, SIMPLIFIED
            if SpaghettiRuntime.IsNameExist (obj, name)
                % RETURN THE VARIABLE
                i = SpaghettiRuntime.FindVariableIndex (obj, name);
                Debug.SpecialLogLine (['Match found at <' int2str(i) '>']);
                
                if (i <= length(obj.memoryTable))
                    var = obj.memoryTable(i);
                end
            else
                out = strcat("NullExceptionError: Tried accessing variable that does not exist. <", (name), ">");
                Debug.Throw (out)
            end
        end

        % FIND A VARIABLE BY NAME, RETURNS INDEX
        function o = FindVariableIndex (obj, name)
            for i = 1:length(obj.memoryTable)
                % SET VARIABLE TO ith ELEMENT
                variable = obj.memoryTable(i);
                
                % ONCE IT MATCHES, BREAK LOOP
                if strcmp(variable.Name, name)
                    Debug.SpecialLogLine (strcat ("MATCHED ", name));
                    o = i;
                    Debug.SpecialLogLine ("Now breaking...");
                    break;
                end

                if strcmp(variable.Name, name)
                    Debug.SpecialLogLine ("Break failed.");
                end
                
                % IT DOESN'T EXIST
                o = 'Does not exist.';
            end
        end

        % FINDING IF VARIABLE WITH NAME EXISTS, RETURNS BOOLEAN
        function o = IsNameExist (obj, name)
            o = false;
            for i = 1:length(obj.memoryTable)
                % SET VARIABLE TO ith ELEMENT
                variable = obj.memoryTable(i);

                % ONCE IT MATCHES, BREAK LOOP
                if strcmp(variable.Name, name)
                    Debug.SpecialLogLine ("Matched");
                    o = true;
                    break;
                end
            end
        end

        % FIND A VARIABLE BY NAME, RETURNS IF IS FUNCTION
        function o = IsNameFunction (obj, name)
            % IF IT EXISTS, DO THIS
            variable = SpaghettiRuntime.GetVariable (obj, name);
            o = variable.isFunc;
        end

    end
end
