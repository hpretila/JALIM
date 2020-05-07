% ======================================================================
% ENUM THAT IMPLEMENTS THE FOLLOWING TO ANALYSE EXPRESSIONS
% 0 - EXPRESSION EXECUTION Where code is run, to return a value if not void
% 1 - LHSRHS INITIALISATION Where assignment and object creation is done
% 1 - 2 - LHSRHS ASSIGNMENT Where assignment is done to an object (LHS) and a scope (RHS)
% 1 - 3 - LHSRHS (RETURN) COMPARISON Where an operator is used on two scopes (LHS,RHS) to return a 0,1 value
% 4 - SCOPED CODE var funcname () {code}
% 5 - CONDITIONAL SCOPED CODE if (code)
% 6 - INPUT SCOPED CODE funcname (code)
% 7 - COMPILER CODE
classdef EnumExpression
    % OCTAVE DOES NOT SUPPORT ENUMERATIONS
    %enumeration
        %EXP_GENERIC (0)
        %EXP_LHSRHS (1)
        %EXP_LHSRHS_ASSIGN (2)
        %EXP_LHSRHS_RETURN (3)
        %EXP_SCOPED (4)
        %EXP_CONDITIONAL (5)
        %EXP_ARGUMENTS (6) 
    %end

    % SEMI IMPLEMENTED VIA METHODS
    methods (Static)
        function o = EXP_GENERIC()
            o = 0;
        end
        function o = EXP_LHSRHS()
            o = 1;
        end
        function o = EXP_LHSRHS_ASSIGN()
            o = 2;
        end
        function o = EXP_LHSRHS_RETURN()
            o = 3;
        end
        function o = EXP_SCOPED()
            o = 4;
        end
        function o = EXP_CONDITIONAL()
            o = 5;
        end
        function o = EXP_ARGUMENTS()
            o = 6;
        end
        function o = EXP_LINKER()
            o = 7;
        end
    end
end
