select * from names where firstname in (firstname);
select * from names where firstname in (firstname, firstname);
select * from names where firstname in (lastname, firstname);
select * from names where firstname in ('Zero',firstname);
select * from names where firstname in (firstname,'Zero');
select * from names where firstname in (lastname, 'Zero');
select * from names where firstname in (lastname, 'Zero', 'Cereal');
select * from names where lastname in (firstname, firstname, lastname, firstname, firstname);
select * from names where 'Cool' in (firstname, firstname, lastname, firstname, firstname);
