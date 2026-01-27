// Premium MoonBit Test Cases
// Contains 8 high-quality test cases covering core functionality

test "add_function_comprehensive" {
  // Comprehensive test for add function
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_edge_cases" {
  // Test edge cases for multiply function
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international" {
  // Test international character support for greet function
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "divide_with_ceil_business_logic" {
  // Test business logic scenarios for ceil division
  // Scenario: Calculate required transport vehicles
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // Verify total capacity
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "complex_calculation_pipeline" {
  // Test complex calculation pipeline
  // Scenario: Calculate project completion time
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // Verify total processing capacity
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
}

test "mathematical_properties_verification" {
  // Verify mathematical properties
  let x = 42
  let y = 17
  
  // Commutative property of addition
  assert_eq(add(x, y), add(y, x))
  
  // Commutative property of multiplication
  assert_eq(multiply(x, y), multiply(y, x))
  
  // Distributive property
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "resource_allocation_optimization" {
  // Resource allocation optimization test
  let total_budget = 10000
  let team_members = 6
  
  // Use ceil division to ensure coverage of all cases
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // Verify total allocation doesn't exceed budget significantly
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "error_handling_and_boundaries" {
  // Error handling and boundary tests
  // Division by zero cases
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // Extreme value tests
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}