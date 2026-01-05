// Azimuth Quantum Computing Tests
// This file contains test cases for quantum computing functionality in the telemetry system

test "quantum bit (qubit) basic operations" {
  // Test qubit initialization
  let qubit_zero = Qubit::new(0)  // |0⟩ state
  let qubit_one = Qubit::new(1)   // |1⟩ state
  
  // Test qubit states
  assert_eq(Qubit::state(qubit_zero), 0)
  assert_eq(Qubit::state(qubit_one), 1)
  
  // Test qubit superposition (simplified representation)
  let superposition = Qubit::superposition(0.5, 0.5)  // Equal superposition
  let (alpha, beta) = Qubit::amplitudes(superposition)
  
  // Verify normalization (|α|² + |β|² = 1)
  let normalization = (alpha * alpha) + (beta * beta)
  assert_true(normalization - 1.0 < 0.001)
}

test "quantum gate operations" {
  // Test Pauli-X gate (NOT gate)
  let qubit_zero = Qubit::new(0)
  let x_gate = QuantumGate::pauli_x()
  let qubit_one = QuantumGate::apply(x_gate, qubit_zero)
  
  assert_eq(Qubit::state(qubit_one), 1)
  
  // Test Pauli-Y gate
  let y_gate = QuantumGate::pauli_y()
  let qubit_y = QuantumGate::apply(y_gate, qubit_zero)
  
  // Test Pauli-Z gate
  let z_gate = QuantumGate::pauli_z()
  let qubit_z = QuantumGate::apply(z_gate, qubit_zero)
  
  // Z gate on |0⟩ should leave it unchanged
  assert_eq(Qubit::state(qubit_z), 0)
  
  // Test Hadamard gate (creates superposition)
  let h_gate = QuantumGate::hadamard()
  let superposition = QuantumGate::apply(h_gate, qubit_zero)
  let (alpha, beta) = Qubit::amplitudes(superposition)
  
  // Hadamard on |0⟩ creates (|0⟩ + |1⟩)/√2
  assert_true(alpha - (1.0 / 2.0.sqrt()) < 0.001)
  assert_true(beta - (1.0 / 2.0.sqrt()) < 0.001)
}

test "quantum entanglement operations" {
  // Test Bell state creation
  let qubit1 = Qubit::new(0)
  let qubit2 = Qubit::new(0)
  
  // Create Bell state |Φ+⟩ = (|00⟩ + |11⟩)/√2
  let bell_state = QuantumEntanglement::create_bell_phi_plus(qubit1, qubit2)
  
  // Test entanglement
  assert_true(QuantumEntanglement::is_entangled(bell_state))
  
  // Test measurement correlation (measuring one qubit determines the other)
  let (measured_q1, measured_q2) = QuantumEntanglement::measure_all(bell_state)
  assert_eq(measured_q1, measured_q2)  // Should be the same
}

test "quantum circuit construction" {
  // Create a simple quantum circuit
  let circuit = QuantumCircuit::new(2)  // 2 qubits
  
  // Add gates to circuit
  QuantumCircuit::add_gate(circuit, 0, QuantumGate::hadamard())  // H on qubit 0
  QuantumCircuit::add_gate(circuit, 1, QuantumGate::pauli_x())   // X on qubit 1
  QuantumCircuit::add_cnot(circuit, 0, 1)  // CNOT with control=0, target=1
  
  // Test circuit properties
  assert_eq(QuantumCircuit::num_qubits(circuit), 2)
  assert_eq(QuantumCircuit::num_gates(circuit), 3)
  
  // Execute circuit
  let initial_qubits = [Qubit::new(0), Qubit::new(0)]
  let result_qubits = QuantumCircuit::execute(circuit, initial_qubits)
  
  assert_eq(result_qubits.length(), 2)
}

test "quantum measurement operations" {
  // Test measurement in computational basis
  let qubit_zero = Qubit::new(0)
  let measurement_zero = QuantumMeasurement::measure(qubit_zero)
  assert_eq(measurement_zero, 0)
  
  let qubit_one = Qubit::new(1)
  let measurement_one = QuantumMeasurement::measure(qubit_one)
  assert_eq(measurement_one, 1)
  
  // Test measurement of superposition (probabilistic)
  let superposition = Qubit::superposition(0.5, 0.5)
  
  // Multiple measurements should follow probability distribution
  let mut zero_count = 0
  let mut one_count = 0
  let num_measurements = 1000
  
  for _ in 0..num_measurements {
    let result = QuantumMeasurement::measure(superposition)
    if result == 0 {
      zero_count = zero_count + 1
    } else {
      one_count = one_count + 1
    }
  }
  
  // Results should be approximately 50/50
  let zero_ratio = zero_count.to_float() / num_measurements.to_float()
  let one_ratio = one_count.to_float() / num_measurements.to_float()
  
  assert_true(zero_ratio > 0.4 && zero_ratio < 0.6)
  assert_true(one_ratio > 0.4 && one_ratio < 0.6)
}

test "quantum error correction codes" {
  // Test simple 3-qubit bit flip code
  let logical_qubit = Qubit::new(1)  // Logical |1⟩
  
  // Encode using bit flip code
  let encoded = QuantumErrorCorrection::encode_bit_flip(logical_qubit)
  assert_eq(QuantumErrorCorrection::num_physical_qubits(encoded), 3)
  
  // Introduce error (flip middle qubit)
  let with_error = QuantumErrorCorrection::introduce_bit_flip(encoded, 1)
  
  // Decode and correct
  let decoded = QuantumErrorCorrection::decode_bit_flip(with_error)
  let corrected_state = Qubit::state(decoded)
  
  assert_eq(corrected_state, 1)  // Should be corrected back to |1⟩
}

test "quantum algorithm implementation" {
  // Test Grover's algorithm (simplified)
  let database_size = 8  // 3 qubits needed
  let marked_item = 5    // Item we're searching for
  
  // Initialize Grover's algorithm
  let grover = QuantumAlgorithm::grover(database_size, marked_item)
  
  // Execute algorithm
  let result = QuantumAlgorithm::execute(grover)
  
  // Result should be the marked item with high probability
  assert_eq(result, marked_item)
  
  // Test Deutsch-Jozsa algorithm (simplified)
  let oracle = QuantumOracle::constant_zero()  // Constant zero function
  let deutsch_jozsa = QuantumAlgorithm::deutsch_jozsa(oracle)
  
  let dj_result = QuantumAlgorithm::execute(deutsch_jozsa)
  assert_eq(dj_result, "constant")  // Should identify as constant
}

test "quantum teleportation protocol" {
  // Test quantum teleportation
  let alice_qubit = Qubit::superposition(0.6, 0.8)  // State to teleport
  let entangled_pair = QuantumEntanglement::create_bell_phi_plus(
    Qubit::new(0), 
    Qubit::new(0)
  )
  
  // Perform teleportation
  let (bob_qubit, classical_info) = QuantumTeleportation::teleport(
    alice_qubit, 
    entangled_pair
  )
  
  // Bob applies corrections based on classical information
  let teleported_qubit = QuantumTeleportation::apply_corrections(
    bob_qubit, 
    classical_info
  )
  
  // Verify teleportation (amplitudes should match original)
  let (orig_alpha, orig_beta) = Qubit::amplitudes(alice_qubit)
  let (tele_alpha, tele_beta) = Qubit::amplitudes(teleported_qubit)
  
  assert_true(orig_alpha - tele_alpha < 0.001)
  assert_true(orig_beta - tele_beta < 0.001)
}

test "quantum key distribution" {
  // Test BB84 protocol (simplified)
  let alice = QuantumCryptography::alice_init()
  let bob = QuantumCryptography::bob_init()
  
  // Alice sends quantum bits to Bob
  let quantum_channel = QuantumCryptography::bb84_send_bits(alice, 8)
  
  // Bob receives and measures
  let bob_measurements = QuantumCryptography::bb84_receive_bits(
    bob, 
    quantum_channel
  )
  
  // Classical communication to compare bases
  let classical_channel = QuantumCryptography::exchange_bases(alice, bob)
  
  // Sift key (keep only matching bases)
  let sifted_key = QuantumCryptography::bb84_sift_key(
    bob_measurements, 
    classical_channel
  )
  
  // Verify key properties
  assert_true(sifted_key.length() > 0)
  assert_true(sifted_key.length() <= 8)  // Should be less than or equal to original
}

test "quantum simulation performance" {
  // Test quantum simulation with increasing qubit count
  let small_system = QuantumSimulator::new(4)   // 4 qubits
  let medium_system = QuantumSimulator::new(8)  // 8 qubits
  let large_system = QuantumSimulator::new(12)  // 12 qubits
  
  // Test state space size (2^n)
  assert_eq(QuantumSimulator::state_space_size(small_system), 16)
  assert_eq(QuantumSimulator::state_space_size(medium_system), 256)
  assert_eq(QuantumSimulator::state_space_size(large_system), 4096)
  
  // Test simulation performance metrics
  let small_metrics = QuantumSimulator::performance_metrics(small_system)
  let medium_metrics = QuantumSimulator::performance_metrics(medium_system)
  let large_metrics = QuantumSimulator::performance_metrics(large_system)
  
  // Memory usage should increase exponentially
  assert_true(small_metrics.memory_usage < medium_metrics.memory_usage)
  assert_true(medium_metrics.memory_usage < large_metrics.memory_usage)
  
  // Computation time should increase with system size
  assert_true(small_metrics.computation_time < medium_metrics.computation_time)
  assert_true(medium_metrics.computation_time < large_metrics.computation_time)
}