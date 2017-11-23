import java.util.Scanner;
import java.math.*;
import java.text.DecimalFormat;

public class ReviseLinearAlgebra {
	public Double[][] convertArrayToDouble(Integer[][] inputArray){
		int rows = inputArray.length; 		
		int cols = inputArray[1].length;	
		Double[][] newMatrix = new Double[rows][cols];
		for(int i=0;i<rows;i++) {
			for(int j=0;j<cols;j++) {
				newMatrix[i][j] = inputArray[i][j].doubleValue();
			}
		}
		return newMatrix;
	}
	
	public Double[][] convertArrayToDouble(Integer[] inputArray){
		int rows = inputArray.length; 		
		Double[][]newMatrix = new Double[rows][2];
		for(int i=0;i<rows;i++) {
				newMatrix[i][0] = inputArray[i].doubleValue();
				newMatrix[i][1] = (double) 0;
			
		}
		return newMatrix;
	}
	public Double[][] convertArrayToDouble(Double[] inputArray){
		int rows = inputArray.length; 		
		Double[][]newMatrix = new Double[rows][2];
		for(int i=0;i<rows;i++) {
				newMatrix[i][0] = inputArray[i].doubleValue();
				newMatrix[i][1] = (double) 0;
			
		}
		return newMatrix;
	}
	
	public boolean matrixIsInRowEchleonForm(Double[][] augMatrix) {
		int rows = augMatrix.length; 		
		int cols = augMatrix[1].length;
		//Find pivot in each row (leading non-zero entry)
		int abovePivotCol, currentPivotCol, currentRow;
		abovePivotCol = 0;
		currentPivotCol = 0;
		currentRow = 0;
		while( (abovePivotCol < currentPivotCol | currentRow == 0 |  currentRow == 1 | currentPivotCol == -1) && currentRow < rows) {
			abovePivotCol = currentPivotCol;
			int i =0;
			while(augMatrix[currentRow][i].shortValue()==0 && i < cols ) {
				i++;
				if(i>=cols) {
					break;
				}
			}
			if(i>=cols) {
				currentPivotCol = -1;
			}else {
				currentPivotCol = i;
			}
			currentRow+=1;
		}
		return !(currentRow < rows);
	}
	public boolean matrixIsInReducedRowEchleonForm(Double[][] augMatrix) {
		int rows = augMatrix.length; 		
		int cols = augMatrix[1].length;
		if(!matrixIsInRowEchleonForm(augMatrix)) {
			return false;
		}
		int currentRow = 0;
		while(currentRow < rows ) {
			//find leading zero:
			int i = 0;
			while(augMatrix[currentRow][i]==0) {
				i++;
				if(i>=cols) {
					i = 0;
					break;
				}
			}
			//check if leading non-zero element (pivot) is invalid for this row
			if(augMatrix[currentRow][i]!=0 && augMatrix[currentRow][i]!=1) {
				return false;
			}
			int j = 0;
			
			while(augMatrix[currentRow][i]!=0 && (augMatrix[j][i]!=1 | j==currentRow)) {
				j++;
				if(j>=rows) {
					break;
				}
			}

			if(j<rows && augMatrix[currentRow][i]!=0) {
				return false;
			}
			currentRow+=1;
		}
		return true;
		
	}
	
	public boolean matrixIsInRowEchleonForm(Integer[][] augMatrix) {
		return matrixIsInRowEchleonForm(convertArrayToDouble(augMatrix));
	}
	public boolean matrixIsInReducedRowEchleonForm(Integer[][] augMatrix) {
		return matrixIsInReducedRowEchleonForm(convertArrayToDouble(augMatrix));
	}
	
	
	public Double[][] simplifyMatrix(Double[][] newMatrix){
		int rows = newMatrix.length; 		
		int cols = newMatrix[1].length;
		int currentRow = 0;
		int largestNegValue = 0;
		boolean isNegative = false;
		Double negativeOne = (double) - 1;
		Double zero = (double) 0;
		//possibleSolutions[solCol] = num.divide(denom,4,BigDecimal.ROUND_HALF_UP);
		
		for(Double row[]: newMatrix) {
			boolean canRowReduce = true;
			Double smallestValue = Double.MAX_VALUE;
			for(Double value: row) {
				if(Math.abs(value)<smallestValue && value!=zero && value!=0 && value!=null) {
					smallestValue = Math.abs(value);
					isNegative = (value < zero | value<0);
				}
			}
			if(isNegative) smallestValue *= negativeOne;
			System.out.println("About to simplify row "+currentRow+" by dividing it by "+smallestValue);
			for(int i=0;i<cols;i++) {
				if(newMatrix[currentRow][i]!=0) newMatrix[currentRow][i] /= smallestValue;
				 
				
			}
			currentRow+=1;
		}
		return newMatrix;
		
	}
	public void rowReduction(Double[][] newMatrix) {
		int rows = newMatrix.length; 		
		int cols = newMatrix[1].length;
		int currentRow = 0;
		Double leadingPivots[][] = new Double[rows][2];		
		int colGroup = 0;
		int currRow = 0;
		int iterations = 0;
		BigDecimal possibleSolutions[] = new BigDecimal[cols-1];
		int numSol = 0;
		boolean eliminationOccurred = true;
		boolean substitutionsOccured = true;
		
		/*
		 *  only will exit below while group if no eliminations and no substitutions occur
		 *  this indicates that no more actions can be done to the matrix
		 */
		
		while(eliminationOccurred | substitutionsOccured) { 
			//Update leading pivot locations
			currentRow = 0;
			for(Double row[]: newMatrix) {
				/*
				 * update leadingPivots after substitution or elimination
				 * leadingPivots[rowNum][0] ==> pivot (leading non-zero value ) for rowNum
				 * leadingPivots[rowNum][1] ==> column for pivot
				 */
				if(eliminationOccurred==true | currentRow==0 | substitutionsOccured==true) {
					currRow = 0;
					while(currRow<rows) {
						int i = 0;
						while(newMatrix[currRow][i]==0) {
							i++;
							if(i>=cols) { //indicates that no pivot exists
								leadingPivots[currRow][0] = (double) 0;
								leadingPivots[currRow][1] = (double) 0;
								break;
							}
						}
						if(i<cols) { //indicates that a pivot exists
							leadingPivots[currRow][0] =  newMatrix[currRow][i]; //sets pivot for this current row = newMatrix[currRow][i] != 0 
							leadingPivots[currRow][1] = (double) i;
						}
						currRow+=1;
					}
					
				}
				
				
				
				/*
				 * How to determine which transformation to apply to matrix ? :
				 * Find pivot column group current row belongs to
				 * Find row p that belong to the same group as current row(so these rows have aligning pivots)
				 * If row p is found ...
				 *   ... y = pivot for row p, x = pivot for current row
				 *   ... y + ax = 0 so find factor a through formula a = - y/ x
				 *   ... apply transformation: current row = ( a * current row) + p
				 */
				eliminationOccurred = false;
				if(currentRow>0) {
					System.out.println("Current matrix: ");
					this.displayMatrix(newMatrix);
					System.out.println();
					colGroup = leadingPivots[currentRow][1].intValue();
					Double factor = null;
					for(int i=0;i<rows;i++) {
						if(leadingPivots[i][1]==colGroup && i!=currentRow) {
							eliminationOccurred = true; //transformation found so row will be reduced
							//Formula: y + xa = 0
							// a = - y / x 
							factor = (double) ((-1 *leadingPivots[i][0] )/leadingPivots[currentRow][0]);
							System.out.println("About to apply formula to matrix: p" + (currentRow+1)+ " = p"+(i+1)+" + "+factor+"( p"+(currentRow+1)+" )");
							for(int j=0;j<cols;j++) {
								newMatrix[currentRow][j] = (factor * newMatrix[currentRow][j]) + newMatrix[i][j];
							}
							System.out.println("Current matrix: ");
							this.displayMatrix(newMatrix);
							System.out.println();
							break;
						}
					}
				}
				/*
				 * search for solutions (xi = val)
				 * ...by searching for row with only two non-zero values where one of them is a b value (A|b matrix)
				 * store solutions in solutions array
				 * if new solution is found
				 * ...store solution in array
				 * ...try to find other solutions by substituting solution to rows
				 */
				
				substitutionsOccured = false;
				
				int testCount = 0;
				int solCol = 0;
				
				for(int i=0;i<rows;i++) {
					testCount = 0;
					for(int l=0;l<(cols-1);l++){
						if(newMatrix[i][l]!=0) {
							testCount+=1;
							solCol = l;	
						}
					}
					//testCount stores number of non-zero values not including b value is found within a row
					if(testCount==1 && possibleSolutions[solCol]==null) { //indicates new solution has been discovered on row i
						BigDecimal num = new BigDecimal(newMatrix[i][cols-1]);
						BigDecimal denom = new  BigDecimal(newMatrix[i][solCol]);
						
						possibleSolutions[solCol] = num.divide(denom,4,BigDecimal.ROUND_HALF_UP);
						System.out.println("Solution x" + solCol + " = " + possibleSolutions[solCol] + " = " + newMatrix[i][cols-1] + " / "  + newMatrix[i][solCol] );
						for(int j=0;j<rows;j++) { //try to substitute found solution on other rows
							//Modify any row that involves this specific solution
							testCount = 0;
							if(newMatrix[j][solCol]!=0 && j!=i) { //indicates there is a possibility that found solution can be substituted into this row
								/*
								 * a = current value within previously found solCol (solution column) for this current row
								 * y = actual value of previously found solution (will be substituted into this current row)
								 * z = b value for this current row
								 * .... + a(y) = z
								 * we need to reset z so z = z - a(y)
								 * set a = 0
								 */
								
								System.out.println("Going to substitute x" + solCol + " = " + possibleSolutions[solCol] + " into row "+j);
								BigDecimal y = possibleSolutions[solCol];
								BigDecimal a = new BigDecimal(newMatrix[j][solCol]); 
								BigDecimal ay = a.multiply(y);
								BigDecimal z = new BigDecimal(newMatrix[j][cols-1]);
								BigDecimal zminus = z.subtract(ay);
								
								newMatrix[j][cols-1] = zminus.doubleValue();
								newMatrix[j][solCol] = (double) 0;
								
								this.displayMatrix(newMatrix);
								System.out.println();
								
							}
						}
					}
				}
					
					
					
				currentRow+=1;
				
				
			}
			
		}

		newMatrix = simplifyMatrix(newMatrix); 
		System.out.println("Final matrix: ");
		this.displayMatrix(newMatrix);
		int i=0;
		
		//Display transposed solution vector
		System.out.print("Solution Vector: { " );
		for(BigDecimal solution: possibleSolutions) {
			System.out.print(solution);
			if(i<possibleSolutions.length-1) {
				System.out.print(", ");
			}
			i++;
		}
		System.out.println(" }^T");
	}
	
	public void displayMatrix(Double[][] newMatrix) {
		int rows = newMatrix.length; //e.g. for ... new Integer[x][y], rows = x
		int cols = newMatrix[1].length; // e.g. for ... new Integer[x][y], cols = y
		String currentElement;
	    DecimalFormat df = new DecimalFormat("##.#");
		int i = 0;
		while(i < cols * rows) {
			if(i%cols==0 && i>0) {
				System.out.println();
			}
			
			currentElement = df.format(newMatrix[i/cols][i%cols]);
			System.out.print(currentElement + " ");
			
			i++;
		}
		System.out.println();
		
	}
	
	public void displayMatrix(Integer[][] newMatrix) {
		displayMatrix(convertArrayToDouble(newMatrix));
	}
	public void displayMatrix(Double[] newMatrix) {
		for(int i=0;i<newMatrix.length;i++) {
			System.out.println(newMatrix[i].shortValue());
		}
		System.out.println();
	}
	int smallestInt(int a,int b) {
		if(a>=b) {
			return b;
		}
		return a;
	}
	/*
	 Matrix A:
	 3 4
	 2 3
	 Matrix B
	 2 3
	 1 2
	 For matrix C: cRows = 2, cCols = 2
	 i < cRows (2) j < cCols (2) k < aRows (2)
	 
	 Trace table:
	 i  j  k  a[i][k]  b[k][j]  c[i][j] 
	 0  0  0  3        2        6
	 0  0  1  4        1        6 + 4
	 0  0  1                    10 ( c[0][0] = 10 )
	 0  1  0  3        3        9
	 0  1  1  4        2        9 + 8
	 0  1  1                    17 ( c[0][1] = 17 )
	 1  0  0  2        2        4
	 1  0  1  3        3        4 + 9
	 1  0  1                    13 ( c[1][0] = 13 )
	 1  1  0  2        1        2
	 1  1  1  3        2        2 + 6
	 1  1  1                    8  ( c[1][1] = 8 ) 
	 
	 so matrix C:
	 10 17
	 13 8
	 */
	public void multiplyMatrix(Double[][] a, Double[][]b) {
		int aRows = a.length;
		int aCols = a[1].length;
		int bRows = b.length;
		int bCols = b[1].length;
		
		int cRows = smallestInt(aRows,bRows);
		int cCols = smallestInt(aCols,bCols);
		Double[][] c = new Double[cRows][cCols];
		System.out.println("Matrix A:");
		displayMatrix(a);
		System.out.println("Matrix B:");
		displayMatrix(b);
		/*
		 * Below algorithm:
		 *  cij = sigma(k=1 to aCols) aik * bjj
		 */
		for(int i=0; i<cRows; i++) {
			for(int j=0; j<cCols; j++) {
				c[i][j] = (double) 0;
				for(int k=0;k<aCols;k++) {
					c[i][j] += (a[i][k] * b[k][j]);
				}
			    /* final c[i][j] = (row i for a) * (col j for b) 
				 * given a = Matrix A (above), b = Matrix B (above), i = 0, j = 0 
				 * row i for a : 3 4
				 * row j for b : 2 1
				 * final c[0][0] = (3 * 2) + (4 * 1) = 10
				 * above loop will compute final c[i][j]
				 * ... k = 0: c[0][0] += (3 * 2)  ( = 6 )
				 * ... k = 1: c[0][0] += (4 * 1)  ( = 10 )
				 */
			}
		}
		
		
		System.out.println("Resulting matrix C, where C = A * B:");
		displayMatrix(c);
		
	}
	
	public void multiplyMatrix(Integer[][] a, Integer[][] b) {
		multiplyMatrix(convertArrayToDouble(a),convertArrayToDouble(b));
	}
	
	public void initialiseMatrix(Double[][] newMatrix){
		int rows = newMatrix.length;
		int cols = newMatrix[1].length;
		
		System.out.println("Enter full matrix as a single line with spaces inbetween each number");
		Scanner readMatrix = new Scanner(System.in);
		int i = 0;
		while(i < cols * rows) {
			
			newMatrix[i/cols][i%cols] = readMatrix.nextDouble();
			i++;
		}
		
	}
	
	public void initialiseMatrix(Double[] newMatrix){
		int rows = newMatrix.length;
		System.out.println("Enter full single-column matrix");
		Scanner readMatrix = new Scanner(System.in);
		for(int i=0;i<rows;i++) {
			newMatrix[i] = readMatrix.nextDouble();
		}
		
	}
	public void initialiseMatrix(Integer[] newMatrix){
		int rows = newMatrix.length;
		System.out.println("Enter full single-column matrix");
		Scanner readMatrix = new Scanner(System.in);
		for(int i=0;i<rows;i++) {
			newMatrix[i] = readMatrix.nextInt();
		}
		
	}
	
	public void initialiseMatrix(Integer[][] newMatrix){
		int rows = newMatrix.length;
		int cols = newMatrix[1].length;
		
		System.out.println("Enter full matrix as a single line with spaces inbetween each number");
		Scanner readMatrix = new Scanner(System.in);
		int i = 0;
		while(i < cols * rows) {
			
			newMatrix[i/cols][i%cols] = readMatrix.nextInt();
			i++;
		}
		
	}
	public double findAngleBetweenVectors(Double[][]p, Double[][]q) {
		//p.q = |p||q|cosA, A = angle between vectors
		int pRows = p.length; 
		int qRows = q.length;
		//p.q = dot product of vectors = sum of p[i]*q[i] from i = 0 to i = N - 1 (N = size of p)
		int cRows = smallestInt(pRows,qRows);
		int dotProduct = 0;
		double normOfP = 0;
		double normOfQ = 0;
		for(int i=0;i<cRows;i++) {
			dotProduct += p[i][0] * q[i][0];
			normOfP += p[i][0] * p[i][0];
			normOfQ += q[i][0] * q[i][0];
			
		}
		normOfP = Math.sqrt(normOfP);
		normOfQ = Math.sqrt(normOfQ);
		double angle = Math.acos( dotProduct / (normOfP * normOfQ) ); // = cos^-1( p.q / |p||q| ) in radians
		angle = 360 * angle/(2 * Math.PI );
		return angle;
	}
	public double findAngleBetweenVectors(Double[][]p, Integer[][]q) {
		return findAngleBetweenVectors(p,convertArrayToDouble(q));
	}
	public double findAngleBetweenVectors(Integer[][]p, Double[][]q) {
		return findAngleBetweenVectors(convertArrayToDouble(p),q);
	}
	public double findAngleBetweenVectors(Integer[][]p, Integer[][]q) {
		return findAngleBetweenVectors(convertArrayToDouble(p),convertArrayToDouble(q));
	}
	public double findAngleBetweenVectors(Integer[]p, Integer[]q) {
		return findAngleBetweenVectors(convertArrayToDouble(p),convertArrayToDouble(q));
	}
	public double findAngleBetweenVectors(Double[]p, Double[]q) {
		return findAngleBetweenVectors(convertArrayToDouble(p),convertArrayToDouble(q));
	}
	public static void main(String[] args) {
		int aRows,aCols,bRows,bCols;
		ReviseLinearAlgebra firstSession = new ReviseLinearAlgebra();
		
		while(true) {
			Scanner readInput = new Scanner(System.in);
			System.out.println("What do you want to do ? Enter ... \n 1 - Multiply vectors \n 2 - Determine angle between vectors \n 3 - Check if vector is in row - echleon form \n 4 - Check if vector is in reduced row echleon form \n 5 - Perform Gaussian elimination on input matrix");
			int userChoice = readInput.nextInt();
			if(userChoice==1) {
				System.out.println("*******************\n Multiply matrices \n*******************\n ");
				System.out.println("Enter the following numbers split by space for matrices A and B: aRows aCols bRows bCols ");
				aRows = readInput.nextInt();
				aCols = readInput.nextInt();
				bRows = readInput.nextInt();
				bCols = readInput.nextInt();
				if(aCols>1 && bCols > 1) {
					Double[][] matrix1 = new Double[aRows][aCols];
					Double[][] matrix2 = new Double[bRows][bCols];
					firstSession.initialiseMatrix(matrix1);
					firstSession.initialiseMatrix(matrix2);
					firstSession.multiplyMatrix(matrix1,matrix2);
				}else if(aCols>1) {
					Double[][] matrix1 = new Double[aRows][aCols];
					Double[] matrix2 = new Double[bRows];
					firstSession.initialiseMatrix(matrix1);
					firstSession.initialiseMatrix(matrix2);
					firstSession.multiplyMatrix(matrix1, firstSession.convertArrayToDouble(matrix2));
				}else if(bCols>1) {
					Double[] matrix1 = new Double[aRows];
					Double[][] matrix2 = new Double[bRows][bCols];
					firstSession.initialiseMatrix(matrix1);
					firstSession.initialiseMatrix(matrix2);
					firstSession.multiplyMatrix(firstSession.convertArrayToDouble(matrix1), matrix2);
				}else {
					Double[] matrix1 = new Double[aRows];
					Double[] matrix2 = new Double[bRows];
					firstSession.initialiseMatrix(matrix1);
					firstSession.initialiseMatrix(matrix2);
					firstSession.multiplyMatrix(firstSession.convertArrayToDouble(matrix1), firstSession.convertArrayToDouble(matrix2));
				}
				
				
				
			}else if(userChoice==2) {
				System.out.println("**********************\n Angle between vectors \n**********************\n ");
				System.out.println("Enter the following numbers split by space for single-column vectors A and B: aRows bRows");
				aRows = readInput.nextInt();
				bRows = readInput.nextInt();
				Double[] matrix1 = new Double[aRows];
				Double[] matrix2 = new Double[bRows]; 
				firstSession.initialiseMatrix(matrix1);
				firstSession.initialiseMatrix(matrix2);
				
				System.out.println("Displaying vector A: ");
				firstSession.displayMatrix(matrix1);
				System.out.println("Displaying vector B: ");
				firstSession.displayMatrix(matrix2);
				firstSession.findAngleBetweenVectors(matrix1, matrix2);
				System.out.println("The angle between vectors A and B is " + firstSession.findAngleBetweenVectors(matrix1, matrix2) + " degrees");
			}else if(userChoice==3) {
				System.out.println("**********************\n Row echleon form \n******************\n ");
				System.out.println("Enter the following numbers split by space for (A|B) matrix: aRows aCols ");
				aRows = readInput.nextInt();
				aCols = readInput.nextInt();
				Double[][] matrix1 = new Double[aRows][aCols];
				firstSession.initialiseMatrix(matrix1);
				System.out.println("Matrix A is in Row Echleon form: " + firstSession.matrixIsInRowEchleonForm(matrix1));
			}else if(userChoice==4) {
				System.out.println("**************************\n Reduced Row echleon form \n**************************\n ");
				System.out.println("Enter the following numbers split by space for (A|B) matrix: aRows aCols ");
				aRows = readInput.nextInt();
				aCols = readInput.nextInt();
				Double[][] matrix1 = new Double[aRows][aCols];
				firstSession.initialiseMatrix(matrix1);
				System.out.println("Matrix A is in Row Echleon form: " + firstSession.matrixIsInReducedRowEchleonForm(matrix1));
			}else if(userChoice==5) {
				System.out.println("***********************\n Gaussian Elimination \n***********************\n ");
				System.out.println("Enter the following numbers split by space for (A|B) matrix: aRows aCols ");
				aRows = readInput.nextInt();
				aCols = readInput.nextInt();
				Double[][] matrix1 = new Double[aRows][aCols];
				firstSession.initialiseMatrix(matrix1);
				firstSession.rowReduction(matrix1);
			}else if(userChoice==6) {
				System.out.println("************\n  Quit \n************\n ");
				System.exit(0);
			}
		}
		
		
	}

}
