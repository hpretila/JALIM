clear

% IMPORTS
addpath ("Lexer");
addpath ("Runtime");
rmpath ("Helper");
addpath ("HelperDebug");

% INFORMATION
fprintf (['Jalim 0.0.1b (May 8 2020, 01:02:31) :: Huey Pretila\n' ...
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