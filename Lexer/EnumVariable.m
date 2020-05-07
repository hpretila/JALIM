% ======================================================================
% ENUM THAT IMPLEMENTS THE FOLLOWING FOR VARIABLES
 % 0 = char, 1 = float, 2 = double, 3 = int, 4 = bool, 5 = void
classdef EnumVariable
    % ENUMS IMPLEMENTED VIA FUNCTIONS
    methods (Static)
        function o = CHAR()
            o = 'char';
        end
        function o = FLOAT()
            o = 'float';
        end
        function o = DOUBLE()
            o = 'double';
        end
        function o = INT()
            o = 'int';
        end
        function o = BOOL()
            o = 'bool';
        end
        function o = VOID()
            o = 'void';
        end
        function o = STRING()
            o = 'string';
        end
        function o = INVALID()
            o = 'invalid';
        end
    end
end