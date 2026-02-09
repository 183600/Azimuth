// 简单的 MoonBit 测试用例
// 包含基本的算术运算测试

test "simple_addition" {
  assert_eq(5, 2 + 3)
  assert_eq(0, 0 + 0)
  assert_eq(-1, -3 + 2)
}

test "simple_multiplication" {
  assert_eq(6, 2 * 3)
  assert_eq(0, 5 * 0)
  assert_eq(-6, -2 * 3)
}

test "simple_comparison" {
  assert_eq(true, 5 > 3)
  assert_eq(false, 3 > 5)
  assert_eq(true, 5 >= 5)
}

test "simple_boolean" {
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(true, true || false)
  assert_eq(false, false || false)
}

test "simple_string" {
  assert_eq("", "" ++ "")
  assert_eq("hello", "hello" ++ "")
  assert_eq("world", "" ++ "world")
  assert_eq("helloworld", "hello" ++ "world")
}

test "simple_math_properties" {
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(a + b, b + a)
  
  // 乘法交换律
  assert_eq(a * b, b * a)
}

test "simple_edge_cases" {
  // 零值操作
  assert_eq(0, 0 + 0)
  assert_eq(0, 0 * 42)
  assert_eq(0, 42 * 0)
  
  // 比较边界
  assert_eq(true, 0 == 0)
  assert_eq(false, 0 > 0)
  assert_eq(false, 0 < 0)
  assert_eq(true, 0 >= 0)
  assert_eq(true, 0 <= 0)
}

test "simple_workflow" {
  // 简单工作流测试
  let items = 25
  let capacity = 6
  let packages = items / capacity
  let remainder = items % capacity
  let final_packages = if (remainder > 0) { packages + 1 } else { packages }
  
  assert_eq(5, final_packages)
}

test "simple_negative_operations" {
  // 负数操作
  assert_eq(-10, -5 + -5)
  assert_eq(0, -5 + 5)
  assert_eq(25, -5 * -5)
  assert_eq(-25, -5 * 5)
}

test "simple_complex_calculation" {
  // 复杂计算测试
  let team_members = 4
  let tasks_per_member = 8
  let total_tasks = team_members * tasks_per_member
  
  let sprint_capacity = 15
  let sprints_needed = total_tasks / sprint_capacity
  let remainder = total_tasks % sprint_capacity
  let final_sprints = if (remainder > 0) { sprints_needed + 1 } else { sprints_needed }
  
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, final_sprints)  // ceil(32/15) = 3
}