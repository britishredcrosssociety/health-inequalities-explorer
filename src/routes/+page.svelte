<script>
    import Header from "$components/Header.svelte";
    import SummaryCharts from "$components/SummaryCharts.svelte";
    import SummaryPlot from "../components/SummaryPlot.svelte";
</script>

<section>
    <div class="wrapper">
        <SummaryPlot />
    </div>
</section>

<style>
    section {
		display: -webkit-box;
		display: -ms-flexbox;
		display: flex;
		-webkit-box-pack: center;
		-ms-flex-pack: center;
		justify-content: center;
        background-position: center;
        background-repeat: no-repeat;
        background-size: cover;
        margin: 0;
            margin-bottom: 20px;
        padding: 0;
	}
    .wrapper {
		width: 100%;
		max-width: 768px;
		margin: 0 16;
	}
</style>