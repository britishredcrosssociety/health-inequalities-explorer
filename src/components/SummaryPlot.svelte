<script>
    import { onMount } from 'svelte';
    import { LayerCake, Svg, Html } from 'layercake';
    import { scaleOrdinal } from 'd3-scale';
    import { group } from 'd3-array';

    import Beeswarm from './layercake/BeeswarmForce.svelte';
    import AxisX from './layercake/AxisX.svelte';

    let data;
    let groupedData;
    let xKey = 'scaled_1_1';
    let yKey = 'variable';
    let zKey = 'variable';
    let titleKey = 'label';
    let seriesColors = ['#000'];

    const r = 6;

    onMount(async () => {
        const response = await fetch('https://humaniverse.r-universe.dev/hiedata/data/england_icb_summary_metrics/json');
        data = await response.json();

        groupedData = Array.from(group(data, d => d.variable), ([key, values]) => ({
            variable: key,
            values: values
        }));
    });
</script>

{#if groupedData}
    {#each groupedData as group}
        <div class="variable-group">
            <h3>{group.variable}</h3>
            <div class="chart-container">
                <LayerCake
                    padding={{ bottom: 15 }}
                    data={group.values}
                    x={xKey}
                    z={zKey}
                    xDomain={[-1, 1]}
                    zScale={scaleOrdinal()}
                    zRange={seriesColors}
                    let:width
                >
                    <Svg>
                        <Beeswarm
                            r={width < 400 ? r / 1.25 : r}
                            strokeWidth={5}
                            xStrength={0.95}
                            yStrength={0.075}
                            getTitle={d => d[titleKey]}
                        />
                        <AxisX/>
                    </Svg>
                </LayerCake>
            </div>
        </div>
    {/each}
{/if}
  
<style>
    /*
      The wrapper div needs to have an explicit width and height in CSS.
      It can also be a flexbox child or CSS grid element.
      The point being it needs dimensions since the <LayerCake> element will
      expand to fill it.
    */
    .chart-container {
      width: 100%;
      height: 100px;
    }
    .variable-group {
        margin-bottom: 2rem;
    }
</style>