
----------------------------------------------------------------------------------------------------
-- Imlpementation of the original (l)inear (s)can (r)egister (a)llocation algorithm
-- web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
----------------------------------------------------------------------------------------------------

local ir = require 'ir'

local function rename_registers(subr, mapping)
  -- Reindex everything according to remapped registers
  for k,ir_stmt in ipairs(subr.statements) do
    local new_reg
    if ir_stmt.register_x then
      new_reg = mapping[ir_stmt.register_x]
      if new_reg then ir_stmt.register_x = new_reg end
    end
    if ir_stmt.register_y then
      new_reg = mapping[ir_stmt.register_y]
      if new_reg then ir_stmt.register_y = new_reg end
    end
    if ir_stmt.register_z then
      new_reg = mapping[ir_stmt.register_z]
      if new_reg then ir_stmt.register_z = new_reg end
    end
    if ir_stmt.register_w then
      new_reg = mapping[ir_stmt.register_w]
      if new_reg then ir_stmt.register_w = new_reg end
    end
    if ir_stmt.argument_regs then
      for i,reg in ipairs(ir_stmt.argument_regs) do
        new_reg = mapping[reg]
        if new_reg then ir_stmt.argument_regs[i] = new_reg end
      end
    end
    if ir_stmt.return_regs then
      for i,reg in ipairs(ir_stmt.return_regs) do
        new_reg = mapping[reg]
        if new_reg then ir_stmt.return_regs[i] = new_reg end
      end
    end
  end
end

-- Computes the maximum lifetime interval for the temporary registers used by the given subroutine
-- Returns an array of register names and their intervals, in order of first start index
local function max_lifetimes(subr)
  local start_idx = { }
  local end_idx = { }
  local regs_by_start = {}
  local lifetimes = { }
  local n = #subr.statements
  -- Search forward to find first definition of each temporary register
  for k=1,n do
    local ir_stmt = subr.statements[k]
    if ir_stmt.return_regs then
      -- This statement overwrites multiple registers
      for i,reg in ipairs(ir_stmt.return_regs) do
        if not start_idx[reg] and ir.isregister(reg) then
          start_idx[reg] = k
          regs_by_start[#regs_by_start+1] = reg
        end
      end
    elseif ir_stmt.register_z then
      -- This statement only overwrites a single register
      local reg_z = ir_stmt.register_z
      if not start_idx[reg_z] and ir.isregister(reg_z) then
        start_idx[reg_z] = k
        regs_by_start[#regs_by_start+1] = reg_z
      end
    end
  end
  -- Search backward to find last reference to each temporary register
  for k=n,1,-1 do
    local ir_stmt = subr.statements[k]
    local reg_w = ir_stmt.register_w
    local reg_x = ir_stmt.register_x
    local reg_y = ir_stmt.register_y
    if reg_w then
      if not end_idx[reg_w] and ir.isregister(reg_w) then
        end_idx[reg_w] = k
      end
    end
    if reg_x then
      if not end_idx[reg_x] and ir.isregister(reg_x) then
        end_idx[reg_x] = k
      end
    end
    if reg_y then
      if not end_idx[reg_y] and ir.isregister(reg_y) then
        end_idx[reg_y] = k
      end
    end
    if ir_stmt.argument_regs then
      for i,reg in ipairs(ir_stmt.argument_regs) do
        if not end_idx[reg] and ir.isregister(reg) then
          end_idx[reg] = k
        end
      end
    end
  end
  -- Fill in missing starts/ends, for sake of correctness.
  for reg,k in pairs(start_idx) do
    if not end_idx[reg] then
      end_idx[reg] = n
    end
  end
  for reg,k in pairs(end_idx) do
    if not start_idx[reg] then
      start_idx[reg] = 1
      table.insert(regs_by_start, 1, reg)
    end
  end
  -- Create list of intervals for each register.
  for i,reg in ipairs(regs_by_start) do
    local k_begin = start_idx[reg]
    local k_end = end_idx[reg]
    -- We're iterating in order of start index, so just append.
    lifetimes[#lifetimes+1] = { reg = reg, k_begin = k_begin, k_end = k_end }
  end
  return lifetimes
end

-- Removes intervals from the given active set which have expired, i.e. k_end < k
local function expire_old(active, free_regs, k)
  local n = #active
  -- Unset expired ranges
  for j=1,n do
    local interval_j = active[j]
    if interval_j.k_end >= k then
      -- This interval is still live, so the rest are too. Exit early
      break
    else
      -- Expired, free the register it's using
      active[j] = nil
      table.insert(free_regs, interval_j.target_reg)
    end
  end
  -- Purge nil values
  local j = 1
  for i=1,n do
    local value = active[i]
    if value then
      if i ~= j then
        active[j] = active[i]
        active[i] = nil
      end
      j = j + 1
    end
  end
end

-- Inserts an interval into a the given active set, sorted by increasing end point
local function insert_interval(active, interval)
  local n = #active
  for j=1,n do
    local interval_j = active[j]
    if interval.k_end <= interval_j.k_end then
      local tmp
      for k=j,n do
        tmp = active[k]
        active[k] = interval
        interval = tmp
      end
      break
    end
  end
  active[n+1] = interval
end

-- Converts the given subroutine into one which heavily prioritizes the first `num_target_regs`
-- registers.
local function lsra(subr, num_target_regs)
  -- Compute coarse lifetime intervals of each register.
  local lifetimes = max_lifetimes(subr)
  for k,interval in ipairs(lifetimes) do
    io.write(interval.reg..' is live on ['..interval.k_begin..', '..interval.k_end..')\n')
  end
  -- List of live variables/intervals, kept in order of increasing `k_end`.
  local active = { }
  -- Initialize list of free "hardware" registers.
  local free_regs = { }
  for k=1,num_target_regs do
    free_regs[k] = ir.register(k-1)
  end
  -- Stack index for the next spilled register
  local spill_index = num_target_regs
  -- Main loop
  for k,interval_i in ipairs(lifetimes) do
    -- Remove intervals which have ended before this one starts
    expire_old(active, free_regs, interval_i.k_begin)
    if #free_regs == 0 then
      -- There's no free hardware registers, so we have to spill to the stack. We do this by
      -- allocating a temporary register that is greater than or equal to `num_target_regs`
      local new_stack_reg = ir.register(spill_index)
      spill_index = spill_index + 1
      -- If the last active interval ends later than this one does, spill it to the stack and steal
      -- its register. Otherwise, spill this one to the stack. Hence, this interval has register
      -- priority if it has a shorter lifetime.
      local last_interval = active[#active]
      if last_interval.k_end > interval_i.k_end then
        -- Remove last interval from active set
        active[#active] = nil
        -- Copy register and add current interval to active set
        interval_i.target_reg = last_interval.target_reg
        insert_interval(active, interval_i)
        -- Spill, assign stack register
        last_interval.target_reg = new_stack_reg
      else
        -- Spill, assign stack register
        interval_i.target_reg = new_stack_reg
      end
    else
      -- Take free hardware register and add current interval to active set
      interval_i.target_reg = table.remove(free_regs, 1)
      insert_interval(active, interval_i)
    end
  end
  -- Construct a mapping table based on intervals' original and assigned registers
  local mapping = { }
  for k,interval in ipairs(lifetimes) do
    mapping[interval.reg] = interval.target_reg
    io.write('map '..interval.reg..' -> '..interval.target_reg..'\n')
  end
  -- Perform mapping
  rename_registers(subr, mapping)
  -- Minimum size of register file may have decreased
  subr.size_registers = spill_index
end

return lsra

