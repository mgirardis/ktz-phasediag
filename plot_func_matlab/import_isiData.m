function [ d, labels ] = import_isiData(filename, startRow)
    %% Import data from text file.
    % Script for importing data from the following text file:
    %
    %    D:\Dropbox\p\doutorado\programas\fortran\findISI_win\run\isi_K.600_d.001_l.001_H.000_th.100.dat
    %
    % To extend the code to different selected data or a different text file,
    % generate a function instead of a script.

    % Auto-generated by MATLAB on 2014/12/07 12:42:44

    %% Initialize variables.
    %filename = 'D:\Dropbox\p\doutorado\programas\fortran\findISI_win\run\isi_K.600_d.001_l.001_H.000_th.000.dat';
    %startRow = 17;

    %% Format string for each line of text:
    %   column1: double (%f)
    %	column2: double (%f)
    %   column3: double (%f)
    %	column4: double (%f)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%f%f%f%f%[^\n\r]';

    %% Open the text file.
    fileID = fopen(filename,'r');

    %% Read columns of data according to format string.
    % This call is based on the structure of the file used to generate this
    % code. If an error occurs for a different file, try regenerating the code
    % from the Import Tool.
    header = textscan(fileID, '%[^\n\r]', startRow-1, 'ReturnOnError', false);
    labels = strsplit(header{1}{end}(2:end), ' ');
    labels = labels(~strcmp(labels,''));
    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'EmptyValue' ,NaN,'ReturnOnError', false);

    %% Close the text file.
    fclose(fileID);

    %% Post processing for unimportable data.
    % No unimportable data rules were applied during the import, so no post
    % processing code is included. To generate code which works for
    % unimportable data, select unimportable cells in a file and regenerate the
    % script.

    %% Create output variable
    d = cell2mat(dataArray(1:(end-1)));
%    d = readmatrix(filename,'NumHeaderLines',startRow-1,'ConsecutiveDelimitersRule','join','LeadingDelimitersRule','ignore','Delimiter',' ','LineEnding','\r\n');
    d(isnan(d(:,1)),:)=[];
end