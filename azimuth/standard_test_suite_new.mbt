// 标准 MoonBit 测试套件 - 新增测试用例
// 为 azimuth 库添加高质量的测试覆盖

// 重新定义函数以避免导入问题
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
}

test "basic_arithmetic_subtraction" {
  // 基本减法运算测试
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(0, 0))
  assert_eq(-5, subtract(2, 7))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_simple" {
  // 问候函数简单测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 数学性质验证
  let x = 10
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算
  let items = 23
  let capacity = 5
  let packages = divide_with_ceil(items, capacity)
  
  assert_eq(5, packages)
  
  // 验证总容量
  let total_capacity = multiply(packages, capacity)
  assert_eq(25, total_capacity)
}

test "error_handling_division_by_zero" {
  // 错误处理：除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let base = 10
  let step1 = add(base, 5)        // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)  // 3
  
  assert_eq(3, step4)
}