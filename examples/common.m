function ivals = getinputsfromfile(fname);
f1 = fopen(fname);
c1 = textscan(f1, `%f`);
src = c1{1};
fclose(f1);
readPtr = 1;
ivals.nlayer = src[readPtr];
readPtr = readPtr + 1;
ivals.tflash = 1e-3 * src[readPtr];
readPtr = readPtr + 1;
for i = 1:3
    ivals.cps[i] = src[readPtr + i];
    ivals.rs[i + 3] = 1e3 * src[readPtr + i + 3];
end
readPtr = readPtr + 6;
ivals.kappas[1] = 1e-2 * src[readPtr + 1];
for i = 2:3
    ivals.kappas[i] = 1e-3 * src[readPtr + i];
end
readPtr = readPtr + 3;
for i = 1:ivals.nlayer
    ivals.lams[i] = 1e3 * src[readPtr + i];
end

