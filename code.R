# Add Libraries
source("Jobshop MILP Library.R")

# Jobs, Machines, Duration
dt = fread("Jobshop Data 6x6.csv")
dt$StartTime = 0
dt$EndTime = 0
dt$Index = 1:nrow(dt)

# Op Sequence Table (for precedence constraint)
pq = OpSeq(dt)

# Job Sequence Table (for no-overlap constraint)
sq = JobSeq(dt)


# Total number variables
# Start times of Jobs in each machine (see dt)
# Job Sequence  - required for Non-Overlap Constraint (sq)
N = nrow(dt)+nrow(sq)

# lpSolve Object - 0 constraints, nrow(dt)+nrow(sq) + 1 variables
# Extra Variable for Makespan
m <- make.lp(0,N+1)

# Cost Function - Make Span (Minimise)
# Make Span = Max End Time - Min Start Time
# set.objfn(model, vector of coefficients of all Xs)
set.objfn(m,c(rep(0,N),1))

# Indices of the last task for all jobs
temp = dt[, .(OpSeq = .N), by = "Job"]
temp = merge(temp,dt[,.(Job,OpSeq,Index)])$Index

# add.constraint(model, coeff, sign, rhs, indexes)
for(i in temp){
  add.constraint(m, c(1,-1),">=", 
                 dt[i,Duration],
                 c(N+1,i))
}

# Qty out of DC - Qty into DC = 0
for(i in 1:nrow(pq)) {
  xhs = dt[Job == pq[i,Job] & OpSeq == pq[i,OpSeq1]]$Duration
  xpos = c(pq[i,Index2], pq[i,Index1])
  add.constraint(m,c(1,-1),">=", xhs, xpos)
}

# Apply integer constraints 
set.type(m, dt$Index,"integer")

# Apply the binary constraints for 
set.type(m, dt$Index,"binary")

M = 1e6

# Apply the 1st overlap constraint
for(i in 1:nrow(sq)) {
  xpos   = c(sq[i,Index1], sq[i,Index2], sq[i,Index])
  rhs1 = dt[Machine == sq[i,Machine] & Job == sq[i,Job1]]$Duration
  add.constraint(m,c(-1, 1, M),">=", rhs1, xpos)
}

# Apply the 2nd overlap constraint
for(i in 1:nrow(sq)) {
  xpos   = c(sq[i,Index1], sq[i,Index2], sq[i,Index])
  rhs1 = dt[Machine == sq[i,Machine] & Job == sq[i,Job2]]$Duration - M
  add.constraint(m,c(1, -1, -M),">=", rhs2, xpos)
}

# Solve & Get Solution
x = proc.time()
lp.control(m, timeout = 20)
lp.control(m, sense = "min")
#lp.control(m, presolve = "rows")

solve(m)
proc.time() - x

# Analyse & Store Results
get.objective(m) 
get.variables(m)

out = get.variables(m)[1:nrow(dt)]
dt$StartTime = out
dt[,EndTime := StartTime + Duration]

out = get.variables(m)[(1+nrow(dt)):N]
sq$SeqVar = out

dt; sq

PlotGanttM(dt)
PlotGanttJ(dt)