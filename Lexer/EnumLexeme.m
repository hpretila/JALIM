% ======================================================================
% ENUM THAT IMPLEMENTS THE FOLLOWING TO ANALYSE CODE TYPES
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
% 9 - BOOLEANS
classdef EnumLexeme
    % ENUMS IMPLEMENTED VIA FUNCTIONS
    methods (Static)
        function o = LEX_IDENT()
            o = 0;
        end
        function o = LEX_ASSIGN()
            o = 1;
        end
        function o = LEX_OPERATOR()
            o = 2;
        end
        function o = LEX_SCOPEARGS()
            o = 3;
        end
        function o = LEX_SCOPENESTED()
            o = 4;
        end
        function o = LEX_SCOPE()
            o = 5;
        end
        function o = LEX_DIGITS()
            o = 6;
        end
        function o = LEX_STRINGS()
            o = 7;
        end
        function o = LEX_VARS()
            o = 8;
        end
        function o = LEX_BOOL()
            o = 9;
        end
        function o = LEX_EMPTY()
            o = 10;
        end
        function o = LEX_IGNORE()
            o = 11;
        end
    end
end