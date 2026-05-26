# bean: Data Thinning of Species Occurrences in Environmental Space

A suite of tools to mitigate sampling bias in species occurrence records
by thinning data in the environmental space (E-space). This process can
improve the accuracy and precision of species distribution models (SDM,
also known as ecological niche models, ENM). The package offers a
data-driven protocol to determine thinning parameters using
kernel-density bandwidth selection. Two thinning methods are provided
(stochastic and deterministic) to reduce over-sampled environmental
conditions and down-weight outlier observations. The name 'bean'
reflects the core principle of the method: each 'pod' (a grid cell in
E-space) is allowed to contain only a limited number of 'beans'
(occurrence points). See Silverman (1986, ISBN:978-0-412-24620-3) and
Rousseeuw and Leroy (2003, ISBN:978-0-471-48855-2) for the underlying
statistical methods.

## See also

Useful links:

- <https://github.com/paanwaris/bean>

- <https://paanwaris.github.io/bean/>

- Report bugs at <https://github.com/paanwaris/bean/issues>

## Author

**Maintainer**: Paanwaris Paansri <paanwaris@vt.edu>
([ORCID](https://orcid.org/0000-0001-9992-098X))

Authors:

- Paanwaris Paansri <paanwaris@vt.edu>
  ([ORCID](https://orcid.org/0000-0001-9992-098X))

- Luis E. Escobar <escobar1@vt.edu>
  ([ORCID](https://orcid.org/0000-0001-5735-2750)) \[contributor\]
