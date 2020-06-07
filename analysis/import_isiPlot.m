function isiPlot = import_isiPlot(filename)
    % importa dados isiPlot gerados por getISIMeshPlot.m
    %   resultado:
    % isiPlot.
    %   x -> matriz de T
    %   y -> matriz de xR
    %   z -> matriz de ISI medio
    %   c -> matriz de cores
    %   alpha -> matriz de transparencias alpha
    
    if (nargin < 1) || isempty(filename)
        isiPlot = struct('fileName', [], 'cLabels', [], 'xLabel', [], 'yLabel', [], 'zLabel', [], 'cLabel', [], 'alphaLabel', [], 'x', [], 'y', [], 'z', [], 'c', [], 'alpha', []);
        return;
    end
    
    if ~isempty(regexpi(filename,'\.mat$')) % if it is a mat file
        isiPlot = load(filename);
        isiPlot = isiPlot.p;
        return
    end

    %% Initialize variables.
    delimiter = '\t';
    startRow = 6;
    isiPlot.fileName = filename;

    %% Format string for each line of text:
    %   column1: double (%f)
    %	column2: double (%f)
    %   column3: double (%f)
    %	column4: double (%f)
    %   column5: double (%f)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%f%f%f%f%f%[^\n\r]';

    %% Open the text file.
    fileID = fopen(filename,'r');
    frewind(fileID);
    
    %% gettind header parameters
    line = textscan(fileID, '%[^\n]', 1, 'ReturnOnError', false); % le primeira linha -- nao eh importante
    col_mat_size = textscan(fileID, '# col_mat_size = %d,%d\n', 1, 'ReturnOnError', false); % tamanho das matrizes de cada coluna
    cLabels = textscan(fileID, '# cLabels = %s\n', 1, 'ReturnOnError', false); % label de cada indice de cor
    plot_labels = textscan(fileID, '# plot_labels = %s\n', 1, 'ReturnOnError', false); % label de cada coluna
    ipFields = textscan(fileID, '%[^\n]', 1, 'ReturnOnError', false); % nome dos campos do isiPlot
    
    col_mat_size = [ col_mat_size{:} ];
    isiPlot.cLabels = strsplit(cLabels{1}{1}, ','); 
    temp = strsplit(plot_labels{1}{1}, {',','->'}, 'CollapseDelimiters', true);
    for i = 1:2:numel(temp)
        fName = [ temp{i}, 'Label' ];
        isiPlot.(fName) = temp{i+1};
    end
    dfNames = strsplit(ipFields{1}{1}(2:end), {'\t',' '}, 'CollapseDelimiters', true);
    if (isempty(dfNames{1}))
        dfNames = dfNames(2:end);
    end
    if (numel(dfNames) > 5)
        error('quantidade de campos identificados no cabecalho difere da qtd de colunas no arquivo');
    end
    
    %% Read columns of data according to format string.
    % This call is based on the structure of the file used to generate this code. If an error occurs for a different file, try regenerating the code from
    % the Import Tool.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines',0, 'ReturnOnError', false);

    %% Close the text file.
    fclose(fileID);

    %% Post processing for unimportable data.
    % No unimportable data rules were applied during the import, so no post processing code is included. To generate code which works for unimportable
    % data, select unimportable cells in a file and regenerate the script.

    %% Allocate imported array to column variable names
    i = 1;
    for label = dfNames
        label = label{1};
        isiPlot.(label) = reshape(dataArray{:, i}, col_mat_size);
        i = i + 1;
    end
%     isiPlot.x = dataArray{:, 1};
%     isiPlot.y = dataArray{:, 2};
%     isiPlot.z = dataArray{:, 3};
%     isiPlot.c = dataArray{:, 4};
%     isiPlot.alpha = dataArray{:, 5};

end