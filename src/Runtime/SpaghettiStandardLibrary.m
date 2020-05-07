% WHERE LANGUAGE OVERRIDEN FUNCTIONS AND VAR NAMES ARE DEFINED
classdef SpaghettiStandardLibrary
    methods (Static)
        % ======================================================================
        % RESERVED CALLS

        % RECEIVE RESERVED CALLS FOR SPAGHETTI RUNTIME AS A STRING ARRAY
        function reservedLexemes = reservedFunctions()
            % INITIATE ARRAY WITH FIRST LEXEME 
            reservedLexemes(1) = {'&&&&'};

            % ------------------------------------------------------------------
            % LANGUAGE FUNCTIONS
            reservedLexemes {length(reservedLexemes) + 1} = ...
                "if";
            reservedLexemes {length(reservedLexemes) + 1} = ...
                "else"; % NOT IMPLEMENTED
            reservedLexemes {length(reservedLexemes) + 1} = ...
                "for"; % NOT IMPLEMENTED
            reservedLexemes {length(reservedLexemes) + 1} = ...
                "while"; % NOT IMPLEMENTED

            % ------------------------------------------------------------------
            % STANDARD CALLS (makeshift stdio.h)
            reservedLexemes {length(reservedLexemes) + 1} = ...
                "printf";
        end

        % WILL RETURN IF A VALUE IS A RESERVED LEXEME
        function o = isReservedLexeme (name)
            % DEFAULT; IT PROBABLY ISN'T A RESERVED LEXEME
            o = false;

            % FOREACH RESERVED LEXEME, CHECK IF THERE IS A MATCH. 
            % IF THERE IS, RETURN TRUE.
            for s = SpaghettiStandardLibrary.reservedFunctions()
                if strcmp(char(cellstr(s)), name)
                    o = true;
                    break;
                end
            end
        end

        % ======================================================================
        % LANGUAGE FUNCTIONS

        function RunReservedFunction (obj, variable, args, scope)
            % SET UP A DEFAULT LEXEME; SOMETIMES IT'S NOT EVEN A
            % RETURNABLE FUNCTION
            if strcmp (variable.Name, "if")
                SpaghettiStandardLibrary.RunIf (obj, variable, args, scope);
            elseif strcmp (variable.Name, "printf")
                SpaghettiStandardLibrary.RunPrintF (obj, variable, args, scope);
            else
                Debug.Throw ('NotImplementedError: Function not implemented')
            end
        end

        function RunIf (obj, ~, args, scope)
            Debug.SpecialLogLine (['RUNNING IF ' char(cellstr(scope))])

            % TURN THE ARGUMENT CODE INTO SPAGHETTIEXPRESSION OBJECTS
            arg_code = args;
            arg_code_Expression = Language.spaghettiGetExpression(...
                StringHelper.cell2CodeStr(arg_code));

            % RUN THE ARGUMENT CODE
            arg_lex = SpaghettiRuntime.RunReturnable (obj, arg_code_Expression);
            Debug.SpecialLogLine (['ARGUMENTS RETURN ' char(cellstr(arg_lex.Data))])

            % TURN THE SCOPED CODE INTO SPAGHETTIEXPRESSION OBJECTS
            code = scope;
            code_Expression = Language.spaghettiGetExpression(...
                StringHelper.cell2CodeStr(code));
                
            % CHECK IF THE ARGUMENT RETURNS IN TRUE
            if (strcmp(arg_lex.Data, "true"))
                % IF IT IS TRUE, RUN THE CODE
                Debug.SpecialLogLine (['ATTEMPTING TO RUN BODY ' char(cellstr(scope))])
                SpaghettiRuntime.RunReturnable (obj, code_Expression);
            end
        end

        function RunPrintF (obj, ~, args, ~)
            % TURN THE ARGUMENT CODE INTO SPAGHETTIEXPRESSION OBJECTS
            arg_code = args;
            arg_code_Expression = Language.spaghettiGetExpression(...
                StringHelper.cell2CodeStr(arg_code));
            arg_code_lexemes = SpaghettiRuntime.RunReturnable (obj, arg_code_Expression);

            disp (StringHelper.codeStr2Str(arg_code_lexemes(1).Data));
        end
    end
end