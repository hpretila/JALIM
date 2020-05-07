classdef Debug
    methods (Static)
        % ======================================================================
        % OBTAINS DISPLAYABLE STRING
        function o = objectToString (object)
            try
                % https://au.mathworks.com/help/matlab/ref/matlab.unittest.diagnostics.constraintdiagnostic.getdisplayablestring.html
                o = matlab.unittest.diagnostics.ConstraintDiagnostic.getDisplayableString(object);
            catch
                % OCTAVE DOESN'T SUPPORT DEBUGGING.
                Debug.LogLine ("Object testing on Octave is not supported. Using type instead.");
                o = class(object);
            end
        end

        % ======================================================================
        % IT EQUATES INPUT FUNCTION AND INCREMENTS INPUT N
        function [o_n, o_testLog, o_fail, o_isFail] = testFunc(nIn, failures, ...                    
            obtainedOutput, expectedOutput)
            o_fail = failures;
            o_isFail = 0;
            n = num2str(nIn);

            % CONVERT NUMBER TO STRING
            if isnumeric(expectedOutput)
                Debug.LogLine ("CALLED")
                obtainedOutput = (num2str(obtainedOutput));
                expectedOutput = (num2str(expectedOutput));
            end

            % CHECKS IF IT IS POSSIBLE TO EQUATE THESE TWO OUTPUTS.
            if size(obtainedOutput) == size(expectedOutput)
                if obtainedOutput == expectedOutput
                    testLog = strcat ("Test (", num2str(n), ") was successful.");
                else
                    try
                        testLog = strcat ("Test (", (n), ") was unsuccessful. Expected [", mat2str(size(expectedOutput)), "] <", ...
                            (expectedOutput), "> but got [", mat2str(size(obtainedOutput)), "] <", (obtainedOutput), ">.");
                    catch
                        % IF strcat DOESN'T SUPPORT THE TYPE, CONVERT IT TO DEBUG STRING
                        testLog = strcat ("Test (", num2str(n), ") was unsuccessful. [", mat2str(size(expectedOutput)), "] <", ...
                            Debug.objectToString(expectedOutput), "> but got [", mat2str(size(obtainedOutput)), "] <", ...
                            Debug.objectToString(obtainedOutput), ">.");
                    end
                    o_fail = failures + 1;
                    o_isFail = 1;
                end
            else
                try
                    testLog = strcat ("Test (", (n), ") was unsuccessful. Expected [", mat2str(size(expectedOutput)), "] <", ...
                        (expectedOutput), "> but got [", mat2str(size(obtainedOutput)), "] <", (obtainedOutput), ">.");
                catch
                    % IF strcat DOESN'T SUPPORT THE TYPE, CONVERT IT TO DEBUG STRING
                    testLog = strcat ("Test (", num2str(n), ") was unsuccessful. [", mat2str(size(expectedOutput)), "] <", ...
                        Debug.objectToString(expectedOutput), "> but got [", mat2str(size(obtainedOutput)), "] <", ...
                        Debug.objectToString(obtainedOutput), ">.");
                end
                o_fail = failures + 1;
                o_isFail = 1;
            end

            o_n = nIn + 1;
            o_testLog = char(testLog);
        end

        % ======================================================================
        % LOGGING
        % SO THAT LOGGING CAN BE ENABLED OR DISABLED
        function Log (string)
            %string = StringHelper.char2Str (string);
            %fprintf (string);
        end
        
        function LogLine (string)
            %string = StringHelper.char2Str (string);
            %fprintf ("%s\n", string);
        end

        % FOR VERY IMPORTANT.
        function SpecialLogLine (string)
            %string = StringHelper.char2Str (string);
            fprintf ("%s\n", string);
        end

        % ======================================================================
        % ERROR HANDLING
        % THROW IS NOT SUPPORTED IN OCTAVE.
        function Throw (string)
            
            % DISPLAY ERROR
            disp (string);
            
            % SUGGEST USER TO EXIT
            disp ("Type 'exit' to quit runtime.");
            keyboard
        end
    end
end