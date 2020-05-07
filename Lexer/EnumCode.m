% ======================================================================
% ENUM THAT IMPLEMENTS THE FOLLOWING TO ANALYSE CODE TYPES
% 0 = NORMAL, DIVIDE BY ';'
% 1 = FUNCTION ARGUMENT, DIVIDE BY ')' AND WILL SKIP ARGUMENTS
% 2 = SCOPED CODE, DIVIDE BY '}' AND WILL SKIP SCOPED CODE
% 3 = COMMENTS, DIVIDE BY '\n' INITIATED BY '//'
% 4 = LINKERCODE1, DIVIDE BY '\n' INITIATED BY '#include'
% 5 = (UNIMPLEMENTED) LINKERCODE2, DIVIDE BY '#enddef' INITIATED BY '#ifdef'
classdef EnumCode
    % OCTAVE DOES NOT SUPPORT ENUMERATIONS
    % enumeration
        % CODE_GENERIC (0)
        % CODE_ARGUMENTS (1)
        % CODE_SCOPED (2)
        % CODE_COMMENTS (3)
        % CODE_LINKER_1 (4)
        % CODE_LINKER_2 (5)
    % end

    % SEMI IMPLEMENTED VIA METHODS
    methods (Static)
        function o = CODE_GENERIC()
            o = 0;
        end
        function o = CODE_ARGUMENTS()
            o = 1;
        end
        function o = CODE_SCOPED()
            o = 2;
        end
        function o = CODE_COMMENTS()
            o = 3;
        end
        function o = CODE_LINKER_1()
            o = 4;
        end
        function o = CODE_LINKER_2()
            o = 5;
        end
    end
end
    