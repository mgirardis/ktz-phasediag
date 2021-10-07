function p = mergePlotStruct(p1, p2, ind1, ind2)
    if nargin < 2
        error('mergePlotStruct:param', 'this method should have at least two input plot structs');
    end
    if nargin < 3 || isempty(ind1)
        ind1 = 1:numel(p1.curves);
    end
    if nargin < 4 || isempty(ind2)
        ind2 = 1:numel(p2.curves);
    end
    p = p1;
    p.curves = p1.curves(ind1);
    p.legend = p1.legend(ind1);
    p.legendValues = p1.legendValues(ind1);
    p.curves = [p.curves, p2.curves(ind2)];
    p.legend = [p.legend, p2.legend(ind2)];
    p.legendValues = [p.legendValues, p2.legendValues(ind2)];
end