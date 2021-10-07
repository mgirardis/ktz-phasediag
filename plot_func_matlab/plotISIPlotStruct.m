function [fHand,axh,th] = plotISIPlotStruct(plotStructArr, faceAlpha, plotInd, plotFunc, cMap, axisArgs, labelArgs, textArgs)
    if (nargin < 6) || (isempty(axisArgs))
        axisArgs = {};
    end
    if (nargin < 7) || (isempty(labelArgs))
        labelArgs = {};
    end
    if (nargin < 8) || (isempty(textArgs))
        textArgs = {};
    end
    if (nargin < 5)
        cMap = [];
    end
    if (nargin < 4) || isempty(plotFunc)
        plotFunc = @pcolor; % may be @pcolor OR @image OR @imagesc
    end
    if ((nargin < 3) || (isempty(plotInd)))
        plotInd = 1:numel(plotStructArr);
    end
    disp('* plotting...');
    axh = gca;
    ind = find(~cellfun(@isempty,regexp({plotStructArr(plotInd).zLabel}, 'Amp', 'once')), 1, 'first');
    if ~isempty(ind)
        if (strcmp(func2str(plotFunc),'pcolor'))
            fHand(1) = plotFunc(plotStructArr(ind).x, plotStructArr(ind).y, plotStructArr(ind).c);
            set(fHand(1), 'EdgeColor', 'none');
        else
            fHand(1) = plotFunc(plotStructArr(ind).x(:), plotStructArr(ind).y(:), plotStructArr(ind).c);
            %set(fHand(1), 'AlphaData', plotStructArr(ind).alpha);
            axis xy
        end
        plotInd(plotInd == ind) = [];
        disp([ '0 -> ', plotStructArr(ind).fileName ]);
    end
    %fHand = zeros(1,length(plotInd));
    k = plotInd(1);
    if (strcmp(func2str(plotFunc),'pcolor'))
        if (exist('fHand','var') == 1)
            m = numel(fHand);
            hold(axh,'on');
        else
            m = 0;
        end
        fHand(m+1) = plotFunc(plotStructArr(k).x, plotStructArr(k).y, plotStructArr(k).c);
        set(fHand(m+1), 'EdgeColor', 'none');
        if (m~=0)
            set(fHand(m+1), 'AlphaData', plotStructArr(k).alpha);
        end
    else
        if (exist('fHand','var') == 1)
            m = numel(fHand);
            hold(axh,'on');
        else
            m = 0;
        end
        fHand(m+1) = plotFunc(plotStructArr(k).x(:), plotStructArr(k).y(:), plotStructArr(k).c);
        if (m~=0)
            set(fHand(m+1), 'AlphaData', plotStructArr(k).alpha);
        end
        axis xy
    end
    disp([ '1 -> ', plotStructArr(k).fileName ]);
    hold(axh,'on');
    k = numel(fHand);
    for i = plotInd(2:end)
        k = k + 1;
        if (strcmp(func2str(plotFunc),'pcolor'))
            fHand(k) = plotFunc(plotStructArr(i).x, plotStructArr(i).y, plotStructArr(i).c);
            set(fHand(k), 'FaceAlpha', faceAlpha, 'EdgeColor', 'none', 'AlphaData', plotStructArr(i).alpha);
        else
            fHand(k) = plotFunc(plotStructArr(i).x(:), plotStructArr(i).y(:), plotStructArr(i).c);
            %set(gca, 'YDir', 'reverse');
            set(fHand(k), 'AlphaData', plotStructArr(i).alpha);
        end
        disp([ num2str(k,'%g'), ' -> ', plotStructArr(i).fileName ]);
    end
    hold(axh,'off');
    if (~isempty(cMap))
        if isa(cMap,'function_handle')
            colormap(cMap(numel(plotStructArr(1).cLabels)));
        else
            colormap(cMap);
        end
    end
    if (~isempty(axisArgs))
        set(axh, axisArgs{:});
    end
    [xld,labelArgs] = getArgValue(labelArgs, 'xdisplacement');
    [yld,labelArgs] = getArgValue(labelArgs, 'ydisplacement');
    xlh = xlabel(axh, ['$',plotStructArr(1).xLabel,'$'], 'Interpreter', 'latex', 'Units', 'Normalized', labelArgs{:});
    ylh = ylabel(axh, ['$',plotStructArr(1).yLabel,'$'], 'Interpreter', 'latex', 'Units', 'Normalized', labelArgs{:});
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
    if (~isempty(plotStructArr(1).cLabels))
        tlab = plotStructArr(1).cLabels;
        pref = '';
        suff = '';
        if (~isempty(textArgs))
            [pref,textArgs] = getArgValue(textArgs, 'prefix', '');
            [suff,textArgs] = getArgValue(textArgs, 'suffix', '');
        end
        if (~isempty(cMap))
            if isa(cMap,'function_handle')
                textColors = 1-cMap(numel(tlab));
            else
                textColors = 1-cMap;
            end
        else
            textColors = zeros(numel(tlab),3);
        end
        for i = 1:numel(tlab)
            ind = (plotStructArr(1).c == i);
            tt = plotStructArr(1).cLabels{i};
            x = mean(plotStructArr(1).x(ind));
            y = mean(plotStructArr(1).y(ind));
            cor = repmat(round(norm(textColors(i,:)) / sqrt(3)), 1, 3);
            th(i) = text(x,y,[pref,tt,suff], 'Interpreter', 'latex', 'Color', cor, textArgs{:});
            th(i).Position(1) = x - sign(x) * th(i).Extent(3)/4;
            th(i).Position(2) = y - sign(y) * th(i).Extent(4)/4;
        end
    else
        th = [];
    end
end


function [val,argsOut,c] = getArgValue(args, arg, default)
    if (nargin < 3)
        default = [];
    end
    argsOut = args;
    ind = find(strcmpi(args,arg));
    if (~isempty(ind))
        val = argsOut{ind+1};
        argsOut(ind+1) = [];
        argsOut(ind) = [];
        c = true;
    else
        val = default;
        c = false;
    end
end