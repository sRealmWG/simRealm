%Root folder
RootFolder = '..\data\simulations\';
%File list
sFile = 'QEAYJ1252R_neutral_sim.csv';
%Create a table for results
Names = ["parameter_id", "T00", "T01", "T11",  "T10"];
Types = ["double", "double", "double", "double", "double"];
FN = [RootFolder sFile];
% file
t = readtable(FN);
lParameter_id = unique(t.parameter_id);
tRes = table('Size', [length(lParameter_id) length(Names)], 'VariableNames', Names, 'VariableTypes', Types);
%for each file
for iP = 1:length(lParameter_id)
    %Get pres/ab data
    tSubSet = t(t.parameter_id==lParameter_id(iP), :);
    tSubSetUnst = unstack(tSubSet, 'n', 'species');
    tSubSetUnst = fillmissing(tSubSetUnst,'constant',0);
    SpAbund = tSubSetUnst{:, 3:end};
    SpAbund = SpAbund >0;%get presence absence 
    spDiff = diff( SpAbund, 1, 1);
    spSum = ( SpAbund(1:end-1, :) +  SpAbund(2:end, :));
    spSum(spSum(:) < 2)=0;
    spTrans = spDiff + spSum;
    [Ts, Grps] = grpstats(spTrans(:), spTrans(:),{ 'numel', 'gname'}) ;
    %-1, T10, 0, T00, 1, T01, 2, T11
    Ind = str2num(char(Grps{:})) + 2;
    Ts = Ts(Ind);
    
    Ts = Ts(Ind([2, 3, 4, 1]));
    Ts([1:2]) = Ts([1:2])/sum(Ts([1:2]));
    Ts([3:4]) = Ts(3:4)/sum(Ts([3:4]));
    
    %get differences
    tRes(iP, {'parameter_id', 'T00', 'T01',  'T11', 'T10'}) = {lParameter_id(iP), Ts(1), Ts(2), Ts(3), Ts(4)};
    %get sum,)
    %set sum to 0 where sum <2
    %diffrnc = d+s  (-1, 0, 1, 2)
    %find the number of transitions
end
%find prbobabilities
[filepath,name,ext] = fileparts(sFile);
ResFN = [RootFolder name, '_Ts.', ext];
writetable(tRes, ResFN)

