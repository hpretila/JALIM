% CODE PREPROCESSED PRIOR TO BEING INTERPRETED; USED FOR INITIAL
% CODE INTERPRETATION AND FOR EVALUATING SCOPES
classdef SpaghettiVariable < handle
    properties
        Name 
        Value % AN ARRAY BY DEFAULT, EMPTY WHEN FUNCTION
        Type % 0 = char, 1 = float, 2 = double, 3 = int, 4 = bool, 5 = void, 6 = string
        isPointer = false % (UNIMPLEMENTED) FALSE
        isArray = false % (UNIMPLEMENTED)

        % FUNCTION SPECIFIC VARIABLES
        Arguments % IF isFunc
        Code % IF isFunc
        isFunc = false % FALSE
    end
end