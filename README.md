# schymrd
mouse_and_rat_hypertension-dataset-explorer
---

### Single Marker View:
Explore a single feature (gene, metadata, etc.) and its relation to variations of clustering or on a per sample basis. 
User can select desired dataset based on tissue type, species and condition. User can select Identity i.e. desired treatment, cell type class levels. 

#### Options: 
![](.README_images/single_marker.png)
- Feature: [Genes]
    - Genes: Are you interested in looking at marker genes?

- Reduction Type: [PCA, harmony, UMAP]
- Identity: 
    - Orig.ident: This will color the graph based on the names of the samples processed. 
    - RNA_snn_res.0.XX: This will color the graph based on groupings produced by Seurat as various resolutions.
        - A higher value of XX means that there is a higher resolution, and therefore more clusters or inferred groups of cell types. 
        - A lower value of XX means that there is a 
- Primary Feature: This will change to be Genes, based on the value selected for ‘Feature’.

#### Graphs:
- The first plot is a UMAP that is colored based on the Primary Feature selection. 
- The second plot is a violin plot that displays the Identity selection on the X-axis and the Primary Feature on the Y-axis. 
- The third plot is the harmony/PCA/UMAP that is colored based on the Identity selection. 

---
### Multiple Marker View:
Explore two features (genes) and its relation to variations of clustering or on a per sample basis. 

#### Options: All of the options here are the same as the Single Marker View with the following field as an option.
![](.README_images/double_marker.png)

- Secondary Feature: This, in combination with the Primary Feature field enables a user to explore two Genes, based on the value selected for ‘Feature’.

#### Graphs:
- The first plot is a UMAP that is colored based on the Primary Feature and Secondary Feature selection. 
- The second plot’s first tile is a violin plot that displays the Identity selection on the X-axis and the Primary
Numeric on the Y-axis. The second plot’s second tile is the same as the first tile but is based on the selection of the Secondary Feature field. 
- The third plot is the UMAP that is colored based on the Identity selection. 

---
### Marker Set (Grid)
This plot helps to explore sets of genes and their relation to the identity. Major Identities include  

#### Options:
![](.README_images/marker_set.png)
- Identity: the same as what is described for the Single Marker View
- Gene Selection: here you choose the set of genes you would like to explore based on the Identity selected. 

#### Graph:
- Y-axis represents the Identity, such as the original samples or some groupings at a certain resolution.
- X-axis represents the genes selected. (Primary Feature) 
- The size of each dot on the grid represents the percentage of cells that expressed that gene. 
- The color intensity of each dot on the grid represents the average expression of the cells that expressed a given gene. 
- So what makes for a good marker gene for some given identity?
    - High mean expression
    - High percentage of cells expressing the gene
    - Low mean expression and percentage of cells expressing the gene for the rest of the identities
    

---
### Cluster Tree Exploration
This plot helps to identify closest related clusters so when moving into the final analysis you have a better idea of 
what the real cell groups are in your samples. 

![](.README_images/cluster_tree.png)
---   
### Modules
###### Documentation
###### Single Marker
###### Multiple Marker
###### Marker Set (Grid)
###### Multiple Feature Plot
###### Cluster Tree
###### Seperated Feature
###### Seperated Categorical
###### Marker Table
###### Download


