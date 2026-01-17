// Azimuth 项目新增标准测试用例
// 使用标准的 MoonBit 测试语法，涵盖各种测试场景

test "string_concatenation" {
  // 测试字符串连接
  assert_eq_string("Hello, World!", "Hello, " + "World!")
  assert_eq_string("MoonBit", "Moon" + "Bit")
  assert_eq_string("12345", "12" + "345")
  assert_eq_string("", "" + "")
}

test "numeric_operations" {
  // 测试数值运算
  assert_eq(10, 2 + 3 + 5)
  assert_eq(100, 10 * 10)
  assert_eq(5, 20 / 4)
  assert_eq(1, 15 % 7)
  assert_eq(8, 2 * 3 + 2)
}

test "boolean_logic" {
  // 测试布尔逻辑
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(true, true || false)
  assert_eq(false, false || false)
  assert_eq(false, !true)
  assert_eq(true, !false)
}

test "comparison_operations" {
  // 测试比较操作
  assert_eq(true, 5 > 3)
  assert_eq(false, 5 > 10)
  assert_eq(true, 5 >= 5)
  assert_eq(false, 5 >= 10)
  assert_eq(true, 3 < 5)
  assert_eq(false, 10 < 5)
  assert_eq(true, 5 <= 5)
  assert_eq(false, 10 <= 5)
  assert_eq(true, 5 == 5)
  assert_eq(false, 5 == 10)
  assert_eq(true, 5 != 10)
  assert_eq(false, 5 != 5)
}

test "array_operations" {
  // 测试数组操作
  let arr = [1, 2, 3, 4, 5]
  assert_eq(3, arr.length())
  assert_eq(1, arr[0])
  assert_eq(5, arr[4])
  
  let empty_arr = []
  assert_eq(0, empty_arr.length())
}

test "conditional_logic" {
  // 测试条件逻辑
  let x = 10
  let y = 20
  
  let result = if (x > y) { x } else { y }
  assert_eq(20, result)
  
  let abs_value = if (x < 0) { -x } else { x }
  assert_eq(10, abs_value)
  
  let is_even = if (x % 2 == 0) { true } else { false }
  assert_eq(true, is_even)
}

test "math_functions" {
  // 测试数学函数
  assert_eq(0, abs(0))
  assert_eq(5, abs(5))
  assert_eq(5, abs(-5))
  
  assert_eq(1, min(1, 5))
  assert_eq(1, min(5, 1))
  assert_eq(-3, min(-3, -1))
  
  assert_eq(5, max(1, 5))
  assert_eq(5, max(5, 1))
  assert_eq(-1, max(-3, -1))
}

test "type_conversions" {
  // 测试类型转换
  assert_eq(42, int("42"))
  assert_eq(0, int("0"))
  assert_eq(-42, int("-42"))
  
  assert_eq_string("42", string(42))
  assert_eq_string("0", string(0))
  assert_eq_string("-42", string(-42))
}

test "error_handling" {
  // 测试错误处理
  let divide_result = try {
    10 / 2
  } catch {
    _ => 0
  }
  assert_eq(5, divide_result)
  
  let array_access = try {
    let arr = [1, 2, 3]
    arr[1]
  } catch {
    _ => -1
  }
  assert_eq(2, array_access)
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算圆的面积：π × r²
  let radius = 5
  let pi = 3.14159
  let circle_area = pi * radius * radius
  assert_eq(78.53975, circle_area)
  
  // 计算二次方程：ax² + bx + c
  let a = 2
  let b = 3
  let c = 1
  let x = 2
  let quadratic = a * x * x + b * x + c
  assert_eq(15, quadratic)
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial = 5 * 4 * 3 * 2 * 1
  assert_eq(120, factorial)
}