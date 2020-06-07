clear all
close all

% dirName{1} = 'data_new';
dirName{1} = 'data\log';
outputDir = dirName{1};
% filePattern{1} = 'ISI_*.dat';
filePattern{1} = 'isi_*d.001_*.dat';
% startRow = 22;
startRow = 17;
xCol = 2;
yCol = 1;
groupISIThresh = 10;
isiFSCSThresh = 20;
isiBSThresh = 80;
%saveMode = 'text';
saveMode = 'mat';

% soh usa os nomes abaixo se saveMode=='mat'
outFileNames = { { 'isiPlot_Tvsd.mat' } };

for i = 1:length(dirName)
    disp(['dir = ', dirName{i}]);
    for j = 1:length(filePattern)
%         if (i == 1) && (j == 1)
%             continue;
%         end
        disp(['  ptrn = ', filePattern{j}]);
        compara_isi_ic(dirName{i}, filePattern{j}, startRow, xCol, yCol, groupISIThresh, isiFSCSThresh, isiBSThresh, outFileNames{i}{j}, saveMode, outputDir);
    end
end