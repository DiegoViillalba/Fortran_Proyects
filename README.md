
# Computational Physics Projects in Fortran

This repository contains Fortran code developed for various numerical problems as part of the Computational Physics course. The projects cover topics such as numerical interpolation, differentiation, integration, eigenvalue calculations, and solving differential equations.

## Repository Contents

### 1. **Project: Secular Equation Calculation**
- **Description**: Numerical solution of the secular equation for a system of 3 coupled springs with positive charges.
- **Techniques Used**:
  - Finite difference method to approximate derivatives.
  - Power method to compute eigenvalues and eigenvectors.
- **Files**:
  - `p1.f90`: Generates the potential matrix using finite differences.
  - `p2.f90`: Calculates eigenvalues and eigenvectors using the power method.
- **Results**:
  - Normal modes obtained: \( \omega_1 = 5 \), \( \omega_2 = 15 \).
  - High precision comparison between numerical and analytical results.

---

### 2. **Task 2: Numerical Interpolation**
- **Description**: Implementation of Newton and Lagrange interpolation methods to analyze experimental data (specific heat of water).
- **Techniques Used**:
  - Polynomial interpolation.
  - Numerical differentiation to analyze minima.
- **Files**:
  - `lagrange.f90`: Implements Lagrange interpolation.
  - `newton.f90`: Implements Newton interpolation.
- **Results**:
  - Comparison of interpolation methods with a maximum difference of \( \Delta = 1.82 	imes 10^{-12} \).

---

### 3. **Task 3: Numerical Differentiation**
- **Description**: Use of finite differences to approximate derivatives and solve differential equations.
- **Techniques Used**:
  - Forward, backward, and centered finite differences.
  - Fourth-order methods for increased accuracy.
- **Files**:
  - `derivada.f90`: Approximates first and second derivatives.
  - `ecuacion_diferencial.f90`: Solves a differential equation with initial conditions.
- **Results**:
  - Graphs of calculated derivatives.
  - Analysis of electric fields in a system with resistance and inductance.

---

### 4. **Task 4: Numerical Integration**
- **Description**: Application of numerical integration methods, such as Simpson's rule, to solve differential equations and calculate areas.
- **Techniques Used**:
  - Composite Simpson's rule.
  - Quadrature formulas for cubic polynomials.
- **Files**:
  - `simpson.f90`: Implements Simpson's rule for uniform intervals.
- **Results**:
  - Calculation of the distance traveled by a car until it stops.
  - Demonstration of the 3/8 rule for cubic polynomial integration.

---

### 5. **Task 5: Eigenvalue Calculation**
- **Description**: Solves the system of three masses coupled with springs using numerical methods.
- **Techniques Used**:
  - Newton interpolation to find the characteristic polynomial.
  - Power and inverse power methods for eigenvalue and eigenvector computation.
- **Files**:
  - `eigenvalores.f90`: Resolves the system of differential equations using eigenvalues.
- **Results**:
  - Normal modes obtained: \( \omega_1 = 0.837 \), \( \omega_2 = 1.612 \), \( \omega_3 = 2.121 \).

---

## Requirements
- **Fortran Compiler**: GNU Fortran (`gfortran`) or similar.
- **Additional Tools**: `gnuplot` for graph generation.

## Usage Instructions
1. Clone this repository:
   ```bash
   git clone https://github.com/your-username/fortran-computational-physics.git
   cd fortran-computational-physics
   ```
2. Compile the desired program:
   ```bash
   gfortran program_name.f90 -o program_name
   ```
3. Run the program:
   ```bash
   ./program_name
   ```
4. (Optional) Use `gnuplot` to plot results.

## Author
**Diego Antonio Villalba González**  
Developed as part of the Computational Physics course at Facultad de Ciencias, UNAM.
