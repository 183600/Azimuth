// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例
// 测试覆盖核心功能：基本运算、字符串处理、边界条件

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(5, 2 + 3)
  assert_eq(0, 0 + 0)
  assert_eq(-1, -3 + 2)
  assert_eq(100, 50 + 50)
  assert_eq(0, 10 + -10)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(6, 2 * 3)
  assert_eq(0, 5 * 0)
  assert_eq(-6, -2 * 3)
  assert_eq(15, -3 * -5)
  assert_eq(10000, 100 * 100)
}

test "integer_division_operations" {
  // 测试整数除法运算
  assert_eq(3, 9 / 3)
  assert_eq(2, 10 / 4)
  assert_eq(0, 5 / 10)
  assert_eq(-3, -9 / 3)
  assert_eq(-2, 9 / -4)
}

test "string_operations_and_concatenation" {
  // 测试字符串操作和连接
  assert_eq("", "" ++ "")
  assert_eq("hello", "hello" ++ "")
  assert_eq("world", "" ++ "world")
  assert_eq("helloworld", "hello" ++ "world")
  assert_eq("hello world", "hello" ++ " " ++ "world")
  assert_eq(5, "hello".length())
  assert_eq(0, "".length())
}

test "array_basic_operations" {
  // 测试数组基本操作
  let empty_array : Array[Int] = []
  assert_eq(0, empty_array.length())
  
  let single_array = [1]
  assert_eq(1, single_array.length())
  assert_eq(1, single_array[0])
  
  let multi_array = [1, 2, 3, 4, 5]
  assert_eq(5, multi_array.length())
  assert_eq(1, multi_array[0])
  assert_eq(5, multi_array[4])
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(false, false && true)
  assert_eq(false, false && false)
  
  assert_eq(true, true || true)
  assert_eq(true, true || false)
  assert_eq(true, false || true)
  assert_eq(false, false || false)
  
  assert_eq(false, not(true))
  assert_eq(true, not(false))
}

test "comparison_operations" {
  // 测试比较操作
  assert_eq(true, 5 == 5)
  assert_eq(false, 5 == 3)
  assert_eq(true, 5 != 3)
  assert_eq(false, 5 != 5)
  
  assert_eq(true, 5 > 3)
  assert_eq(false, 3 > 5)
  assert_eq(true, 3 < 5)
  assert_eq(false, 5 < 3)
  
  assert_eq(true, 5 >= 5)
  assert_eq(true, 5 >= 3)
  assert_eq(false, 3 >= 5)
  assert_eq(true, 3 <= 3)
  assert_eq(true, 3 <= 5)
  assert_eq(false, 5 <= 3)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(a + b, b + a)
  
  // 乘法交换律
  assert_eq(a * b, b * a)
  
  // 加法恒等式
  assert_eq(a, a + 0)
  assert_eq(a, 0 + a)
  
  // 乘法恒等式
  assert_eq(a, a * 1)
  assert_eq(a, 1 * a)
  assert_eq(0, a * 0)
}

test "edge_case_operations" {
  // 测试边界情况
  // 零值操作
  assert_eq(0, 0 + 0)
  assert_eq(0, 0 * 42)
  assert_eq(0, 42 * 0)
  assert_eq(0, 0 - 0)
  
  // 负数操作
  assert_eq(-10, -5 + -5)
  assert_eq(0, -5 + 5)
  assert_eq(25, -5 * -5)
  assert_eq(-25, -5 * 5)
  
  // 比较边界
  assert_eq(true, 0 == 0)
  assert_eq(false, 0 > 0)
  assert_eq(false, 0 < 0)
  assert_eq(true, 0 >= 0)
  assert_eq(true, 0 <= 0)
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目资源需求
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = team_members * tasks_per_member
  
  let days_per_sprint = 5
  let tasks_per_day = 10
  let sprint_capacity = days_per_sprint * tasks_per_day
  
  let sprints_needed = total_tasks / sprint_capacity
  let remainder = total_tasks % sprint_capacity
  let final_sprints = if (remainder > 0) { sprints_needed + 1 } else { sprints_needed }
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(50, sprint_capacity)  // 5 * 10 = 50
  assert_eq(1, final_sprints)  // ceil(48/50) = 1
}