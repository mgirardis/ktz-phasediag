function [ r, pY, pX ] = averageISI(isiMat, dataLabels, xCol, yCol, isiThreshold)
% plota o ISI num diagrama 2D usando o isi dado na matriz isiMat,
% na coluna isiCol, sendo a intensidade de cada isi dada na coluna
% intCol da mesma matriz
% isiThreshold separa dois ISI diferentes para o mesmo par xR,T
%
% isiMat eh a matrix de 4 colunas diretamente no arquivo gerado por isi.exe
% coluna 1 -> parametro B
% coluna 2 -> parametro A
% coluna 3 -> ISI
% coluna 4 -> intensidade do ISI (densidade de probabilidade de achar aquele ISI)
%
% dataLabels = sao os labels das colunas do arquivo gerado por isi.exe
% no arquivo padrao, tem:
% dataLabels = { 'xR', 'T', 'ISI', 'intensity' }
%
% xCol = 2 (indice da coluna da matriz isiMat contendo os valores do eixo x)
% yCol = 1 (indice da coluna da matriz isiMat contendo os valores do eixo y)
%
% isiThreshold = 10 tipicamente para separar os ISI (pode testar qualquer valor)
%
    isiCol = find(strcmp(dataLabels, 'ISI'));
    intCol = find(strcmp(dataLabels, 'intensity'));
    pXLabel = dataLabels{xCol};
    pYLabel = dataLabels{yCol};
    [~,k] = sort(isiMat(:,1),'ascend');
    isiMat = isiMat(k,:);
    pX = unique(isiMat(:,xCol), 'stable');
    [pY,k1] = unique(isiMat(:,1), 'first');
    k2 = [k1(2:end)-1;size(isiMat,1)];
    if yCol == 1
        m = length(pX);
        n = length(pY);
    else
        m = length(pY);
        n = length(pX);
    end
    r.(pXLabel) = zeros(1, m*n);
    r.(pYLabel) = zeros(1, m*n);
    r.ISIAvg = zeros(1, m*n);
    r.ISIStd = zeros(1, m*n);
    r.n = zeros(1, m*n);
    r.Dist = repmat(struct('ISI',[],'P',[]), 1, m*n);
    k = 0;
    for j = 1:n
        %matTemp = isiMat(isiMat(:,yCol) == pY(j),:);
        matTemp = isiMat(k1(j):k2(j),:);
        u2 = unique(matTemp(:,2), 'first');
        for i = 1:numel(u2)
            ind = matTemp(:,2) == u2(i);
            k = k + 1;
            isiTemp = matTemp(ind,isiCol);
            intTemp = matTemp(ind,intCol);
            r.ISIAvg(k) = sum(isiTemp .* intTemp);
            %r.ISIStd(k) = std(isiTemp);
            r.ISIStd(k) = sqrt(sum(intTemp.*isiTemp.^2) - r.ISIAvg(k).^2);
            r.(pXLabel)(k) = pX(i);
            r.(pYLabel)(k) = pY(j);
            r.n(k) = countISIGroups(matTemp(ind,isiCol),isiThreshold);
            r.Dist(k).ISI = matTemp(ind,isiCol)';
            r.Dist(k).P = matTemp(ind,intCol)';
        end
    end
end

function n = countISIGroups(isi, isiThresh)
    isi = sort(isi);
    m = length(isi);
    n = 1;
    for i = 2:m
        if ((isi(i) - isi(i-1)) > isiThresh)
            n = n + 1;
        end
    end
end