SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;


/*==========================================================================================================================================
  ==========================================================================================================================================

--Task 1: Understanding the data in hand

--A. Describe the data in hand in your own words.

Ans: The superstoredb contains information about the various customers, the different orders placed by them,
	 the products being sold by the store and the overall market fact information.
	
	Given below are the information about the various tables that are imported into the "superstoresdb" schema
	 using the .csv files that were provided.


cust_dimen - This table contains the information about the various customers.

ColumnName   		DataType		Meaning
----------			-------	   		---------
Cust_id				text	   		This is a unique identifier and this value cannot be NULL.
Customer_Name		text			Name of the Customer, need not be unique.
Customer_Segment	text			The segment that the customer belongs to.(CONSUMER, SMALL BUSINESS, HOME OFFICE, CORPORATE)
Province			text			The Area the Customer belongs to.
Region				text			The Region the Customer belongs to.
	
orders_dimen - This table contains the details of the orders made by customers.

ColumnName			DataType		Meaning
----------			-------			---------
Order_ID			int(11)			This is not the Unique identifier because of duplicate values.
Order_Date			Date			The date when the order was placed.
Order_Priority		text			The priority of the Order. (HIGH, MEDIUM, LOW, NOT SPECIFIED)
Ord_id				text			This is a unique identifier and this value cannot be NULL.

prod_dimen - The table contains the details about all the products that are being sold.

ColumnName				DataType	Meaning
----------				-------		---------
Prod_id					text		This is a unique identifier and this value cannot be NULL.
Product_Category		text		The various categories of products sold. (OFFICE SUPPLIES, TECHNOLOGY, FURNITURE)
Product_Sub_Category 	text		Each Product_Category is further divided into Product_Sub_Category.

shipping_dimen - The file contains shipping details for the placed orders by the customers.

ColumnName		DataType			Meaning
----------		-------				---------
Ship_id			text				This is a unique identifier and this value cannot be NULL.
Ship_Mode		text				The Orders are shipped by three modes : REGULAR AIR, DELIVERY TRUCK, and EXPRESS AIR
Ship_Date		text				The Date when shipment was started.
Order_ID		int(11)				The Order reference for each Shipment.

market_fact - This table contains the information about each order,the customers who placed the order, products bought, quantity, and profits, etc.

ColumnName				DataType	Meaning
----------				------		---------
Ord_id					text		This refernces the Order_ID from shipping_dimen
Prod_id					text		This refernces the Prod_id from prod_dimen
Ship_id					text		This refernces the Ship_id fromshipping_dimen
Cust_id					text		This refernces the Cust_id from cust_dimen
Sales					double		The sales amount for an Order
Discount				double		The discount amount for an Order
Order_Quantity      	int(11)		The quantity of products sold for an Order
Profit              	double		The profit amount received for an Order
Shipping_Cost       	double		The shipment cost for an order
Product_Base_Margin     double		The base margin value for a Product


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

--B. Identify and list the Primary Keys and Foreign Keys for this dataset

Ans: Below are the various tables along with their respective Primary and Foreign Keys.

#1. cust_dimen (Customer_Name, Province, Region, Customer_Segment, Cust_id)
	Primary keys	: 	Cust_id
	Foreign keys	: 	Not Present
	
#2. orders_dimen (Order_ID, Order_Date, Order_Priority, Ord_id)
	Primary keys	:	Ord_id
	Foreign keys	:	Not Present

#3. prod_dimen (Product_Category, Product_Sub_Category, Prod_id)
	Primary keys	:	Prod_id
	Foreign keys	:	Not Present

#4. shipping_dimen (Order_ID, Ship_Date, Ship_id, Ship_Mode)
	Primary keys	:	Ship_id
	Foreign keys	:	Not Present
	
#5. market_fact (Ord_id, Prod_id, Ship_id, Cust_id, Sales, Discount, Order_Quantity, Profit, Shipping_Cost, Product_Base_Margin)
	Primary keys	:	Not Present
	Foreign keys	:	Ord_id, Prod_id, Ship_id, Cust_id
*/
/*==========================================================================================================================================
  ==========================================================================================================================================*/

##Task 2: Basic Analysis

#A Find the total and the average sales (display total_sales and avg_sales)
#Ans:
	SELECT SUM(Sales) AS total_sales, AVG(Sales) AS avg_sales 
	FROM market_fact;

#B Display the number of customers in each region in decreasing order of no_of_customers. 
#The result should contain columns Region, no_of_customers
#Ans:
	SELECT Region, COUNT(*) AS no_of_customers
	FROM cust_dimen
	GROUP BY Region
	ORDER BY no_of_customers DESC;

#C Find the region having maximum customers (display the region name and max(no_of_customers)
#Ans:	
	##Method 1: (Same as previous question, but limiting to first record)
	SELECT Region, COUNT(*) AS no_of_customers
	FROM cust_dimen
	GROUP BY Region
	ORDER BY no_of_customers DESC
	LIMIT 1;

	##Method 2: (using HAVING clause)
	SELECT 
    Region, COUNT(*) AS no_of_customers
	FROM cust_dimen
	GROUP BY Region
	HAVING no_of_customers = (SELECT MAX(no_of_customers) FROM (SELECT COUNT(*) AS no_of_customers FROM cust_dimen GROUP BY Region) tmp_cust_dimen); 

#D Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)
#Ans: 
	SELECT Prod_id AS 'product id', SUM(Order_Quantity) AS no_of_products_sold
	FROM market_fact
	GROUP BY Prod_id
	ORDER BY no_of_products_sold DESC;

#E Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased 
#(display the customer name, no_of_tables purchased) 
#Ans:
	SELECT cd.Customer_Name, SUM(mf.Order_Quantity) AS no_of_tables_purchased
	FROM market_fact mf 
			INNER JOIN
		 cust_dimen cd ON mf.Cust_id=cd.Cust_id AND cd.Region= "ATLANTIC"
		 	INNER JOIN 
		 prod_dimen pd ON pd.Prod_id=mf.Prod_id AND pd.Product_Sub_Category= "TABLES"
    GROUP BY cd.Customer_Name
    ORDER BY no_of_tables_purchased DESC;
	
/*==========================================================================================================================================
  ==========================================================================================================================================*/

##Task 3: Advanced Analysis

#A Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?
#Ans:
	SELECT pd.Product_Category AS product_category, SUM(mf.Profit) AS profits
	FROM market_fact mf
			INNER JOIN 
		 prod_dimen pd ON pd.Prod_id=mf.Prod_id
	GROUP BY pd.product_category
	ORDER BY profits DESC;

#B Display the product category, product sub-category and the profit within each subcategory in three columns.
#Ans:
	SELECT pd.Product_Category AS 'product_category', pd.Product_Sub_Category AS 'product_sub_category', SUM(mf.Profit) AS 'profits_of_sub_category' 
	FROM market_fact mf
			INNER JOIN 
		 prod_dimen pd ON pd.Prod_id=mf.Prod_id
	GROUP BY product_category , product_sub_category; 
	

#C Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, 
#  display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region,
#  no_of_shipments, profit_in_each_region)
#Ans:
	
	# Query to retrieve least profitable product sub-category
	SELECT pd.product_sub_category, SUM(mf.Profit) AS total_profit
	FROM prod_dimen pd 
			INNER JOIN
		 market_fact mf ON pd.Prod_id=mf.Prod_id
	GROUP BY pd.product_sub_category
	ORDER BY total_profit 
	LIMIT 1;
    
	#The Least profitable sub caegory is found to be "TABLES"

    #Query to retrieve the Region where the least profitable product subcategory i.e "TABLES" is shipped in decreasing order of profits.
    SELECT cd.Region AS Region, COUNT(mf.Ship_id) AS no_of_shipments, SUM(mf.Profit) as profit_in_each_region
    FROM  cust_dimen cd
    		INNER JOIN
    	  market_fact mf ON mf.Cust_id=cd.Cust_id
    	  	INNER JOIN
    	  prod_dimen pd ON pd.Prod_id=mf.Prod_id AND pd.Product_Sub_Category="TABLES"
    GROUP BY Region
    ORDER BY profit_in_each_region DESC;

    # ONTARIO is the region where the least profitable product subcategory "TABLES" is shipped the most.
/*==========================================================================================================================================    
  ==========================================================================================================================================*/