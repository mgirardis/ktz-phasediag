function [x,y,z,c,alpha,cLabels] = getISIMeshPlot(isiAvg, xLabel, yLabel, zLabel, isiThresh, isiBThresh)
    m = length(unique(isiAvg.(xLabel), 'stable'));
    n = length(unique(isiAvg.(yLabel), 'stable'));
    x = reshape(isiAvg.(xLabel), m, n)';
    y = reshape(isiAvg.(yLabel), m, n)';
    z = reshape(isiAvg.(zLabel), m, n)';
    c = zeros(size(isiAvg.(zLabel)));
    alpha = 0.8.*ones(size(c));
    
    indFP = isiAvg.ISIAvg == 0;
    indFS = isiAvg.n == 1 & (isiAvg.ISIAvg > 0 & isiAvg.ISIAvg < isiThresh);
    %indPCS = isiAvg.n == 1 & isiAvg.ISIAvg >= isiThresh;
    %indACS = findACS(isiAvg, 1, isiThresh);
    indPCS = isiAvg.ISIStd < 4 & isiAvg.ISIAvg >= isiThresh;
    indACS = findACS(isiAvg, 4, isiThresh);
    indB = findBursting(isiAvg, 1, isiThresh, isiBThresh);
    indSB = findSlowBursting(isiAvg, 1, isiThresh, isiBThresh);
    c(indFP) = 1;
    c(indFS) = 2;
    c(indPCS) = 3;
    c(indACS) = 4;
    c(indB) = 5;
    c(indSB) = 6;
    c(c==0) = 7;
    c = reshape(c, m, n)';
    cLabels = { 'FP', 'FS', 'PCS', 'ACS', 'B', 'SB', 'Rest' };    
    
    alpha(indFP) = 0.5;
    alpha = reshape(alpha, m, n)';
end

function ind = findBursting(isiAvg, nThresh, isiThresh, isiBThresh)
    n = length(isiAvg.ISIAvg);
    ind = [];
    if (iscell(isiAvg.Dist))
        getDist = @(i) isiAvg.Dist{i};
    else
        getDist = @(i) isiAvg.Dist(i);
    end
    for i = 1:n
        if (isiAvg.n(i) > nThresh)
            tempDist = getDist(i);
            %temp = sort(tempDist.ISI);
            %if ((temp(1) < isiThresh) && ((temp(end) - temp(1)) > isiBThresh))
            temp = minmax(tempDist.ISI);
            if ((temp(1) < isiThresh) && ((temp(2) - temp(1)) > isiBThresh))
                ind(end+1) = i;
            end
        end
    end
end

function ind = findSlowBursting(isiAvg, nThresh, isiThresh, isiBThresh)
    n = length(isiAvg.ISIAvg);
    ind = [];
    if (iscell(isiAvg.Dist))
        getDist = @(i) isiAvg.Dist{i};
    else
        getDist = @(i) isiAvg.Dist(i);
    end
    for i = 1:n
        if ((isiAvg.n(i) > nThresh) && (isiAvg.ISIAvg(i) > isiBThresh))
            tempDist = getDist(i);
            %temp = sort(tempDist.ISI);
            %if ((temp(1) < isiThresh) && ((temp(end) - temp(1)) > isiBThresh))
            temp = minmax(tempDist.ISI);
            if ((temp(1) < isiThresh) && ((temp(2) - temp(1)) > isiBThresh))
                ind(end+1) = i;
            end
        end
    end
end

function ind = findACS(isiAvg, nThresh, isiThresh)
    n = length(isiAvg.ISIAvg);
    ind = [];
    if (iscell(isiAvg.Dist))
        getDist = @(i) isiAvg.Dist{i};
    else
        getDist = @(i) isiAvg.Dist(i);
    end
    for i = 1:n
        %if (isiAvg.n(i) > nThresh)
        if (isiAvg.ISIStd(i) > nThresh)
            temp = getDist(i);
            if (all(temp.ISI > isiThresh))
                ind(end+1) = i;
            end   
        end
    end
end