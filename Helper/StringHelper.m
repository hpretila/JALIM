classdef StringHelper
    methods (Static)
        % CONVERTS STRINGCELL INTO A ROW-CHAR ADDS NEW LINES AND ETC THAT THE INTERPRETER CAN READ
        function obj = cell2CodeStr(cellIn) 
            o = StringHelper.char2Str (cellIn);

            % SPECIAL NEWLINE CHARACTER
            o = regexprep (o, '[\n]', char (182));
            o = regexprep (o, '\n', char (182));
            o = regexprep (o, "\n", char (182));
            o = strrep (o, '[\n]', char (182));
            o = strrep (o, '\n', char (182));
            o = strrep (o, "\n", char (182));
            %o = regexprep (o, "$", char (182));
            o = char(o);

            % RETURN
            obj = o;
        end

        function obj = codeStr2Str(cellIn) 
            o = cellIn;

            % SPECIAL NEWLINE CHARACTER
            o = regexprep (o, char (182), '[\n]');
            o = regexprep (o, char (182), '\n');
            o = regexprep (o, char (182), "\n");
            o = regexprep (o, char (187), ' ');
            o = strrep (o, char (182), '[\n]');
            o = strrep (o, char (182), '\n');
            o = strrep (o, char (182), "\n");
            o = strrep (o, char (187), ' ');
            o = char(o);

            % RETURN
            obj = o;
        end

        % CONVERTS
        function obj = char2Str (cellIn)
            % CONVERT THE CODE TO AN ACCEPTABLE TYPE
            cellType = ['' class(cellIn)];

            % OCTAVE CAN SOMETIMES IMPORT FILE DATA AS A STRUCT OBJECT
            if strcmp (cellType, 'struct')
                codeIn = cellIn.textdata;
            else
                codeIn = cellIn;
            end

            % SET DEFAULT VALUE OF o
            o = char(codeIn);

            % IF THE STRING IS NOT A 1 ROW VECTOR
            codeMatrixSize = size (codeIn);
            if 1 < codeMatrixSize(1)
                o = '';
                % WORK THROUGH EACH ROW
                row = 1;
                while (row <= codeMatrixSize(1))
                    % AND EACH COLUMN
                    column = 1;
                    while (column <= codeMatrixSize(2))
                        % COLUMN VALUE
                        columnVal = codeIn(row,column);
                        columnVal = char(columnVal);
                        
                        % APPEND COLUMN VALUE
                        o = [o columnVal];

                        % INCREMENT COLUMNS
                        column = column + 1;
                    end
                    % APPEND LINEBREAK CHARACTER
                    o = [o char(10)];

                    % INCREMENT ROWS
                    row = row + 1;
                end
            else
                codeIn = char (codeIn);
                % IF IT HAS LINEBREAKS, SPLIT AND RECURSE
                if 1 < length(strsplit (codeIn, char(10)))
                    splitString = cellstr (strsplit (codeIn, char(10)));
                    o = char2Str (splitString);
                end
            end
            
            o = char(o);

            % RETURN
            obj = o;
        end
    end
end