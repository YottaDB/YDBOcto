-- Public Domain
-- Taken from https://www.nlm.nih.gov/research/umls/rxnorm/sourcereleasedocs/rxnorm.html
-- TSPEED006 - Speed Test RxNorm Sample 1
SELECT rxcui, str as rxnorm_str, tty as rxnorm_tty, suppress
FROM rxnconso
WHERE code = '310965' AND sab = 'RXNORM';
