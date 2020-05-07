% CONTAINER FOR CODE PREPROCESSED PRIOR TO BEING INTERPRETED; USED FOR 
% INITIAL CODE INTERPRETATION AND FOR EVALUATING SCOPES
classdef SpaghettiExpression < handle
    properties % (access = public) BY DEFAULT
        RawCode % A string
        Code % An array of Lexemes

        % EXPRESSION GUIDE
        % 0 - EXPRESSION EXECUTION Where code is run, to return a value if not void
        % 1 - LHSRHS INITIALISATION Where assignment and object creation is done
        % 1 - 2 - LHSRHS ASSIGNMENT Where assignment is done to an object (LHS) and a scope (RHS)
        % 1 - 3 - LHSRHS (RETURN) COMPARISON Where an operator is used on two scopes (LHS,RHS) to return a 0,1 value
        % 4 - SCOPED CODE var funcname () {code}
        % 5 - CONDITIONAL SCOPED CODE if (code)
        % 6 - INPUT SCOPED CODE funcname (code)
        ExpressionType

        % CODE TYPE GUIDE
        % 0 = NORMAL, DIVIDE BY ';'
        % 1 = FUNCTION ARGUMENT, DIVIDE BY ')' AND WILL SKIP ARGUMENTS
        % 2 = SCOPED CODE, DIVIDE BY '}' AND WILL SKIP SCOPED CODE
        % 3 = COMMENTS, DIVIDE BY '\n' INITIATED BY '//'
        % 4 = LINKERCODE1, DIVIDE BY '\n' INITIATED BY '#include'
        % 5 = (UNIMPLEMENTED) LINKERCODE2, DIVIDE BY '#enddef' INITIATED BY '#ifdef'
        CodeType = EnumCode.CODE_GENERIC()

        % GETTING SCOPE INFORMATION
        HasScope
        HasArguments
        isComplex
    end
    methods
        % ======================================================================
        % OPERATIONS

        % SET THE CODE CONTAINED BY EXPRESSION
        function obj = SetCode (obj, inCode, expressionType)
            % CLEAN UP inCode
            
            
            % SET RAWCODE
            obj.RawCode = char(cellstr(inCode));
            obj.ExpressionType = expressionType;
            
            % SPLIT UP DEBUG CODE
            Debug.LogLine ('=======================================');
            Debug.LogLine (['Now calculating expression: ' inCode]);

            % RETURN A LEXEME ARRAY, IF EXPRESSION TYPE ISN'T COMPILER CODE
            if expressionType ~= EnumExpression.EXP_LINKER()
                obj.Code = SpaghettiLexeme.parseExpression(obj.RawCode, obj.ExpressionType);
            end
        end

        % ======================================================================
        % FUNCTION OVERLOADS

        % EQUATION OVERLOAD a == b
        function o = eq(a,b)
            o = true;

            % CHECK IF SIZES MATCH
            if size(a) == size(b)
                % CHECK FOR EVERY ELEMENT
                for i = 1:size(a)
                    % COMPARE CODE VALUES
                    if ~strcmp(a(1).RawCode, b(1).RawCode)
                        o = false;
                        break;
                    end
                end
            else
                o = true;
            end
        end
    end
end