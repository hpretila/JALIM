clear

% TURN OFF ALL WARNINGS (OCTAVE)
warning('off','all');

% IMPORTS
addpath ("Lexer");
addpath ("Runtime");
rmpath ("HelperDebug");
addpath ("Helper");

% INFORMATION
fprintf (['Jalim 0.0.1b (May 4 2020, 16:59:59) :: Huey Pretila\n' ...
'Type "leave" to end interpreter.\n']);

% INITIALISE RUNTIME
runtime = SpaghettiRuntime;
runtime.Init();

% RECEIVE INPUT FROM THE USER
x = "";
while (true)
    x = input('>>> ','s');
    
    % IF USER ASKS TO LEAVE
    if strcmp(x, 'leave')
        % LEAVEs
        break;
    % OTHERWISE PARSE THE CODE
    else
        runtime.Parse(x);
    end
end