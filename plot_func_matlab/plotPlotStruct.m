function h = plotPlotStruct(axh, pStruct, lineSet, widthSet, symbolSet, colorSet, plotArgs, errArgs, axisArgs, legendArgs, labelArgs, titleArgs)
    if isempty(axh)
        axh = gca;
    end
    if ((nargin < 3) || (isempty(lineSet)))
        lineSet = { '-' };
    end
    if ((nargin < 4) || (isempty(widthSet)))
        widthSet = 1;
    end
    if ((nargin < 5) || (isempty(symbolSet)))
        symbolSet = 'o';
    end
    if ((nargin < 6) || (isempty(colorSet)))
        colorSet = [0,0,1];
    end
    if ((nargin < 7) || (isempty(plotArgs)))
        plotArgs = {};
    end
    if ((nargin < 8) || (isempty(errArgs)))
        errArgs = {};
    end
    if ((nargin < 9) || (isempty(axisArgs)))
        axisArgs = {};
    end
    if ((nargin < 10) || (isempty(legendArgs)))
        legendArgs = {};
    end
    if ((nargin < 11) || (isempty(labelArgs)))
        labelArgs = {};
    end
    if ((nargin < 12) || (isempty(titleArgs)))
        titleArgs = {};
    end

    getLineSt = @(ii)getPLine(ii,lineSet);
    getColor = @(ii)getPColor(ii,colorSet);
    getSymbol = @(ii)getPSymbol(ii,symbolSet);
    getWidth = @(ii)widthSet(int32(mod(int32(ii)-1,numel(widthSet)))+1);
    
    [val, errArgs, co] = getArgValue(errArgs, 'showerrorbar');
    if (~co)
        showErrorBar = true;
    else
        showErrorBar = strcmpi(val, 'on');
    end
    [errLineSpec,errArgs] = getArgValue(errArgs, 'linespec');
    [plotLineSpec,plotArgs,emptyLineSpec] = getArgValue(plotArgs, 'linespec');
    emptyLineSpec = ~emptyLineSpec;
    
    hold(axh, 'all');
    nC = numel(pStruct.curves);
    h = cell(1, nC);
    %hv = [];
    for i = 1:nC
        if (emptyLineSpec)
            plotLineSpec = [getLineSt(i),getSymbol(i)];
        end
        if (pStruct.curves(i).showLegend)
            legvis = 'on';
        else
            legvis = 'off';
        end
        if (showErrorBar && ~isempty(pStruct.curves(i).yErr))
            h{i}=plotErrAx(axh, pStruct.curves(i).x, pStruct.curves(i).y, [], [pStruct.curves(i).yErr.L(:),pStruct.curves(i).yErr.U(:)],...
                plotLineSpec, errLineSpec,...
                [ { 'LineWidth', getWidth(i), 'Color', getColor(i), 'MarkerFaceColor', getColor(i), 'HandleVisibility', legvis } plotArgs ], errArgs);
        else
            hold(axh, 'all');
            temp = [ { 'LineWidth', getWidth(i), 'Color', getColor(i), 'MarkerFaceColor', getColor(i), 'HandleVisibility', legvis } plotArgs ];
            h{i}=plot(axh, pStruct.curves(i).x, pStruct.curves(i).y, plotLineSpec, temp{:});
        end
        %hv((numel(hv)+1):(numel(hv)+numel(h{i})-1)) = h{i}(1:(end-1));
    end
    for i = 1:nC
        uistack(h{i}(end), 'top');
        %hv(end+1) = h{i}(end);
    end
    %uistack(hv);
    hold(axh, 'off');
    if (~isempty(axisArgs))
        set(axh, axisArgs{:});
    end
    legInd = logical([pStruct.curves(:).showLegend]);
    if (any(legInd))
        legend(axh, pStruct.legend(legInd), 'Interpreter', 'latex', legendArgs{:});
    end
    [xld,labelArgs] = getArgValue(labelArgs, 'xdisplacement');
    [yld,labelArgs] = getArgValue(labelArgs, 'ydisplacement');
    xlh = xlabel(axh, pStruct.xLabel, 'Interpreter', 'latex', 'Units', 'Normalized', labelArgs{:});
    ylh = ylabel(axh, pStruct.yLabel, 'Interpreter', 'latex', 'Units', 'Normalized', labelArgs{:});
    xlp = get(xlh, 'Position');
    ylp = get(ylh, 'Position');
    if isempty(xld)
        xld = zeros(1,3);
    end
    if isempty(yld)
        yld = zeros(1,3);
    end
    set(xlh, 'Position', xlp + [0,0.02,0] + xld);
    set(ylh, 'Position', ylp + [0.01,-0.05,0] + yld);
    set(axh, 'XScale', pStruct.xScale, 'YScale', pStruct.yScale);
    if isfield(pStruct, 'title')
        if ~isempty(pStruct.title)
            [titleDispl,titleArgs] = getArgValue(titleArgs, 'displacement');
            [titleUnits,titleArgs] = getArgValue(titleArgs, 'units');
            th = title(axh, pStruct.title);
            if ~isempty(titleUnits)
                set(th, 'Units', titleUnits);
            end
            if ~isempty(titleDispl)
                th.Position = th.Position + titleDispl;
            end
            if ~isempty(titleArgs)
                set(th, titleArgs{:});
            end
        end
    end
    h = axh;
end

function [val,argsOut,c] = getArgValue(args, arg)
    argsOut = args;
    ind = find(strcmp(strCellToLower(args),arg));
    if (~isempty(ind))
        val = argsOut{ind+1};
        argsOut(ind+1) = [];
        argsOut(ind) = [];
        c = true;
    else
        val = [];
        c = false;
    end
end

function s = getPLine(ii, lineSet)
    s = lineSet{int32(mod(int32(ii)-1,numel(lineSet)))+1};
    if ((strcmp(s,'n')) || (strcmp(s,'none')))
        s = '';
    end
end

function s = getPSymbol(ii, symbolSet)
    s = symbolSet(int32(mod(int32(ii)-1,numel(symbolSet)))+1);
    if ((strcmp(s,'n')) || (strcmp(s,'none')))
        s = '';
    end
end

function s = getPColor(ii, colorSet)
    s = colorSet(int32(mod(int32(ii)-1,int32(round(numel(colorSet)/3))))+1,:);
end

function cc = strCellToLower(c)
    n = numel(c);
    cc = cell(size(c));
    for i = 1:n
        if (isstr(c{i}))
            cc{i} = lower(c{i});
        else
            cc{i} = c{i};
        end
    end
end

function hv = plotErrAx(axh, x, y, xe, ye, lineS, errLineS, plotArgs, errArgs)
%ploterr(gca, parPlot.curves(1).x, parPlot.curves(1).y, [], [parPlot.curves(1).yErr.L(:),parPlot.curves(1).yErr.U(:)]);
    x = x(:);
    y = y(:);
    nD = numel(x);
    
    hasXErr = ~isempty(xe);
    hasPlotArgs = nargin >= 8 && ~isempty(plotArgs);
    hasErrArgs = nargin >= 9 && ~isempty(errArgs);
    if ((nargin < 6) || (isempty(lineS)))
        lineS = '-b';
    end
    if ((nargin < 7) || (isempty(errLineS)))
        errLineS = '';
    end
    
    [mm,nn] = size(ye);
    if (nn == nD)
        ye = ye';
    end
    if (mm == 1)
        yeb = ye;
        yet = ye;
    else
        yeb = ye(:,1);
        yet = ye(:,2);
    end
    if (hasXErr)
        [mm,nn] = size(xe);
        if (nn == nD)
            xe = xe';
        end
        if (mm == 1)
            xel = xe;
            xer = xe;
        else
            xel = xe(:,1);
            xer = xe(:,2);
        end
    end

    hv = [];
    hold(axh, 'all');
    for i = 1:nD
        if (hasXErr)
            if (hasErrArgs)
                hv(end+1) = plot(axh, [(x(i) - xel(i)) (x(i) + xer(i))], [y(i) y(i)], errLineS, 'HandleVisibility', 'off', errArgs{:});
            else
                hv(end+1) = plot(axh, [(x(i) - xel(i)) (x(i) + xer(i))], [y(i) y(i)], errLineS, 'HandleVisibility', 'off');
            end
            %uistack(h1, 'bottom');
        end
        if (hasErrArgs)
            hv(end+1) = plot(axh, [x(i) x(i)], [(y(i) - yeb(i)) (y(i) + yet(i))], errLineS, 'HandleVisibility', 'off', errArgs{:});
        else
            hv(end+1) = plot(axh, [x(i) x(i)], [(y(i) - yeb(i)) (y(i) + yet(i))], errLineS, 'HandleVisibility', 'off');
        end
        %uistack(h2, 'bottom');
    end

    if (hasPlotArgs)
        hv(end+1) = plot(axh, x, y, lineS, plotArgs{:});
    else
        hv(end+1) = plot(axh, x, y, lineS);
    end
    hold(axh, 'off');
end