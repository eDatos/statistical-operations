package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.testutil.common.AbstractBusClientServerTestBase;
import org.junit.BeforeClass;
import org.junit.Test;
import org.siemac.metamac.common.test.rest.ServerResource;

public class StatisticalOperationsRestFacadeTest extends AbstractBusClientServerTestBase {

    private static final String PORT = ServerResource.PORT;

    @BeforeClass
    public static void startServers() throws Exception {
        // JNDI SDMXDS
        // SimpleNamingContextBuilder simpleNamingContextBuilder = OracleJNDIMock.setUp("SDMXDS", "metamac", "metamac", "jdbc:oracle:thin:@localhost:1521:XE", null);

        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));
    }

    @Test
    public void testRetrieveOperationByCode() throws Exception {

        // http://localhost:9001/api/internal/v1.0/operations/[CODE]
        String baseAddress = "http://localhost:" + PORT + "/api/internal/v1.0/operations/CODE-1/";
        WebClient webClient = WebClient.create(baseAddress).accept("application/json");
        Response response = webClient.get();
        assertNotNull(response.getEntity().toString());
        // Long id = wc.type("application/xml").accept("text/plain").post(new Book("CXF", 1L), Long.class);
        // assertEquals(new Long(1), id);
        // Book book = wc.accept("application/xml").query("id", 1L).get(Book.class);
        // assertEquals("CXF", book.getName());

        // Path("datastructure/{agencyID}/{resourceID}/{version}")

        // String endpointAddress = "http://localhost:" + PORT + "/webapp/rest/datastructure/INE/KF_DIV_6/1.0";
        //
        // URL url = new URL(endpointAddress);
        // URLConnection connect = url.openConnection();
        // connect.addRequestProperty("Accept", "application/vnd.sdmx.structure+xml;version=2.1");
        // InputStream in = connect.getInputStream();
        // assertNotNull(in);
        // System.out.println(getStringFromInputStream(in));

        // InputStream expected = getClass().getResourceAsStream("resources/expected_get_book123.txt");

        // assertEquals(getStringFromInputStream(expected), getStringFromInputStream(in));

    }

    // private String getStringFromInputStream(InputStream in) throws Exception {
    // CachedOutputStream bos = new CachedOutputStream();
    // IOUtils.copy(in, bos);
    // in.close();
    // bos.close();
    // //System.out.println(bos.getOut().toString());
    // return bos.getOut().toString();
    // }

    @Test
    public void testFindOperations() throws Exception {

        // http://localhost:9001/api/internal/v1.0/operations
        String baseAddress = "http://localhost:" + PORT + "/api/internal/v1.0/operations";
        WebClient webClient = WebClient.create(baseAddress).accept("application/json");
        Response response = webClient.get();
        assertNotNull(response.getEntity().toString());
        
        webClient = WebClient.create(baseAddress).accept("application/xml");
        response = webClient.get();
        assertNotNull(response.getEntity().toString());

    }
}
