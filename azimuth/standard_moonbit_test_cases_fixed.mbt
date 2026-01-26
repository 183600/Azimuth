// 标准 MoonBit 测试用例 - 新增
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 基本加法函数测试
  inspect(add(7, 8))?
  inspect(add(0, 0))?
  inspect(add(-7, -8))?
  inspect(add(-7, 8))?
  inspect(add(1000000, 1000000))?
}

test "multiply_function_basic" {
  // 基本乘法函数测试
  inspect(multiply(7, 8))?
  inspect(multiply(7, 0))?
  inspect(multiply(-7, 8))?
  inspect(multiply(-7, -8))?
  inspect(multiply(1000, 1000))?
}

test "greet_function_basic" {
  // 基本问候函数测试
  inspect(greet("世界"))?
  inspect(greet("München"))?
  inspect(greet("São Paulo"))?
  inspect(greet("🌙"))?
  inspect(greet("あ"))?
}

test "divide_with_ceil_basic" {
  // 基本向上取整除法测试
  inspect(divide_with_ceil(58, 12))?
  inspect(divide_with_ceil(60, 12))?
  inspect(divide_with_ceil(0, 5))?
  inspect(divide_with_ceil(5, 0))?
  inspect(divide_with_ceil(1, 1))?
}

test "subtract_function_basic" {
  // 基本减法函数测试
  inspect(subtract(7, 8))?
  inspect(subtract(7, -8))?
  inspect(subtract(-7, 8))?
  inspect(subtract(-7, -8))?
  inspect(subtract(1000000, 1000000))?
}

test "logistics_calculation" {
  // 物流计算测试
  let packages = 58
  let capacity_per_container = 12
  let containers_needed = divide_with_ceil(packages, capacity_per_container)
  inspect(containers_needed)?
  
  let total_capacity = multiply(containers_needed, capacity_per_container)
  inspect(total_capacity)?
}

test "project_resource_calculation" {
  // 项目资源计算测试
  let team_size = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  inspect(total_tasks)?
  
  let tasks_per_sprint = 20
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  inspect(sprints_needed)?
  
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  inspect(total_capacity)?
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 24
  let y = 18
  
  // 加法交换律
  inspect(add(x, y))?
  inspect(add(y, x))?
  
  // 乘法交换律
  inspect(multiply(x, y))?
  inspect(multiply(y, x))?
  
  // 分配律
  inspect(multiply(x, add(y, 5)))?
  inspect(add(multiply(x, y), multiply(x, 5)))?
}

test "error_handling" {
  // 错误处理测试
  inspect(divide_with_ceil(15, 0))?
  inspect(divide_with_ceil(-15, 0))?
  inspect(divide_with_ceil(0, 0))?
  inspect(divide_with_ceil(1, 1))?
  inspect(divide_with_ceil(-1, -1))?
  inspect(divide_with_ceil(0, 1))?
}

test "event_planning" {
  // 活动规划测试
  let attendees = 95
  let seats_per_row = 12
  let rows_needed = divide_with_ceil(attendees, seats_per_row)
  inspect(rows_needed)?
  
  let total_seats = multiply(rows_needed, seats_per_row)
  inspect(total_seats)?
  
  let remaining_seats = subtract(total_seats, attendees)
  inspect(remaining_seats)?
}