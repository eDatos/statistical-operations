package org.siemac.metamac.statistical_operations.rest.external.v1_0.utils;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.math.BigInteger;

import org.joda.time.DateTime;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.ChildLinks;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.ResourceInternal;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.ClassSystems;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.CollMethods;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Costs;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Families;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Family;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.FreqColls;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.InformationSuppliers;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Instance;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.InstanceTypes;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Instances;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Operation;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Operations;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Producers;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Publishers;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.RegionalContributors;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.RegionalResponsibles;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.SecondarySubjectAreas;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.StatConcDefs;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.StatisticalUnits;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Status;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.SurveySources;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.SurveyTypes;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Measures;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.UpdateFrequencies;
import org.siemac.metamac.statistical_operations.rest.external.RestExternalConstants;
import org.siemac.metamac.statistical_operations.rest.external.v1_0.mapper.Do2RestExternalMapperV10Impl;
import org.siemac.metamac.statistical_operations.rest.external.v1_0.service.StatisticalOperationsRestExternalFacadeV10Test;

public class StatisticalOperationsRestMocks {

    private final String statisticalOperationsInternalApiBase;
    private final String srmInternalApiBase;

    public StatisticalOperationsRestMocks(String statisticalOperationsInternalApiBase, String srmInternalApiBase) {
        assertNotNull(statisticalOperationsInternalApiBase);
        assertNotNull(srmInternalApiBase);

        this.statisticalOperationsInternalApiBase = statisticalOperationsInternalApiBase;
        this.srmInternalApiBase = srmInternalApiBase;
    }

    public Operation mockOperation1() {
        return mockOperation(statisticalOperationsInternalApiBase, "1");
    }

    public Operation mockOperation2() {
        return mockOperation(statisticalOperationsInternalApiBase, "2");
    }

    public Operation mockOperation3() {
        return mockOperation(statisticalOperationsInternalApiBase, "3");
    }

    public Operation mockOperation4() {
        return mockOperation(statisticalOperationsInternalApiBase, "4");
    }

    public Operation mockOperation5() {
        return mockOperation(statisticalOperationsInternalApiBase, "5");
    }

    public Operation mockOperation6() {
        return mockOperation(statisticalOperationsInternalApiBase, "6");
    }

    public Family mockFamily1() {
        return mockFamily(statisticalOperationsInternalApiBase, "1");
    }

    public Family mockFamily2() {
        return mockFamily(statisticalOperationsInternalApiBase, "2");
    }

    public Instance mockInstance1() {
        return mockInstance(statisticalOperationsInternalApiBase, "1", "operation1");
    }

    public Operations mockOperations(String baseApi, String limit, String offset) {
        Operations operations = new Operations();
        operations.setKind(RestExternalConstants.KIND_OPERATIONS);
        operations.setTotal(BigInteger.valueOf(10));
        if (limit == null && (offset == null || "0".equals(offset))) {
            operations.getOperations().add(mockOperation1Resource());
            operations.getOperations().add(mockOperation2Resource());
            operations.getOperations().add(mockOperation3Resource());
            operations.getOperations().add(mockOperation4Resource());
            operations.getOperations().add(mockOperation5Resource());
            operations.getOperations().add(mockOperation6Resource());
            operations.getOperations().add(mockOperation7Resource());
            operations.getOperations().add(mockOperation8Resource());
            operations.getOperations().add(mockOperation9Resource());
            operations.getOperations().add(mockOperation10Resource());
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(25));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            operations.getOperations().add(mockOperation1Resource());
            operations.getOperations().add(mockOperation2Resource());
            operations.getOperations().add(mockOperation3Resource());
            operations.getOperations().add(mockOperation4Resource());
            operations.getOperations().add(mockOperation5Resource());
            operations.getOperations().add(mockOperation6Resource());
            operations.getOperations().add(mockOperation7Resource());
            operations.getOperations().add(mockOperation8Resource());
            operations.getOperations().add(mockOperation9Resource());
            operations.getOperations().add(mockOperation10Resource());
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(1000));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            operations.getOperations().add(mockOperation1Resource());
            operations.getOperations().add(mockOperation2Resource());
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(baseApi + "/operations?limit=2&offset=2");
            operations.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            operations.getOperations().add(mockOperation3Resource());
            operations.getOperations().add(mockOperation4Resource());
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/operations?limit=2&offset=0");
            operations.setNextLink(baseApi + "/operations?limit=2&offset=4");
            operations.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "8".equals(offset)) {
            operations.getOperations().add(mockOperation9Resource());
            operations.getOperations().add(mockOperation10Resource());
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/operations?limit=2&offset=6");
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operations;
    }

    public Operations mockOperations(String baseApi, String limit, String offset, String query) {
        Operations operations = new Operations();
        operations.setKind(RestExternalConstants.KIND_OPERATIONS);
        String querySupported1 = StatisticalOperationsRestExternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1;
        String querySupported2 = StatisticalOperationsRestExternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM;
        if (querySupported1.equals(query)) {
            operations.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                operations.getOperations().add(mockOperation1Resource());
                operations.getOperations().add(mockOperation10Resource());
                operations.setOffset(BigInteger.valueOf(0));
                operations.setLimit(BigInteger.valueOf(25));
                operations.setFirstLink(null);
                operations.setPreviousLink(null);
                operations.setNextLink(null);
                operations.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                operations.getOperations().add(mockOperation1Resource());
                operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                operations.setFirstLink(null);
                operations.setPreviousLink(null);
                operations.setNextLink(baseApi + "/operations?query=" + query + "&limit=1&offset=1");
                operations.setLastLink(baseApi + "/operations?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                operations.getOperations().add(mockOperation3Resource());
                operations.getOperations().add(mockOperation4Resource());
                operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                operations.setFirstLink(baseApi + "/operations?query=" + query + "&limit=1&offset=0");
                operations.setPreviousLink(baseApi + "/operations?query=" + query + "&limit=1&offset=0");
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else if (querySupported2.equals(query)) {
            operations.setTotal(BigInteger.valueOf(1));
            if (limit == null && (offset == null || "0".equals(offset))) {
                operations.getOperations().add(mockOperation1Resource());
                operations.setOffset(BigInteger.valueOf(0));
                operations.setLimit(BigInteger.valueOf(25));
                operations.setFirstLink(null);
                operations.setPreviousLink(null);
                operations.setNextLink(null);
                operations.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return operations;
    }

    public Operations mockOperationsByFamily1(String baseApi, String limit, String offset) {
        Operations operations = new Operations();
        operations.setKind(RestExternalConstants.KIND_OPERATIONS);
        operations.setTotal(BigInteger.valueOf(6));
        if (limit == null && (offset == null || "0".equals(offset))) {
            operations.getOperations().add(mockOperation1Resource());
            operations.getOperations().add(mockOperation2Resource());
            operations.getOperations().add(mockOperation3Resource());
            operations.getOperations().add(mockOperation4Resource());
            operations.getOperations().add(mockOperation5Resource());
            operations.getOperations().add(mockOperation6Resource());
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(25));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            operations.getOperations().add(mockOperation1Resource());
            operations.getOperations().add(mockOperation2Resource());
            operations.getOperations().add(mockOperation3Resource());
            operations.getOperations().add(mockOperation4Resource());
            operations.getOperations().add(mockOperation5Resource());
            operations.getOperations().add(mockOperation6Resource());
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(1000));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            operations.getOperations().add(mockOperation1Resource());
            operations.getOperations().add(mockOperation2Resource());
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            operations.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            operations.getOperations().add(mockOperation3Resource());
            operations.getOperations().add(mockOperation4Resource());
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=4");
            operations.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            operations.getOperations().add(mockOperation5Resource());
            operations.getOperations().add(mockOperation6Resource());
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "7".equals(offset)) {
            // no results
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operations;
    }

    public Operations mockOperationsByFamily2(String baseApi, String limit, String offset) {
        Operations operations = new Operations();
        operations.setKind(RestExternalConstants.KIND_OPERATIONS);
        operations.setTotal(BigInteger.valueOf(4));
        if ("1".equals(limit) && "2".equals(offset)) {
            operations.getOperations().add(mockOperation8Resource());
            operations.setOffset(BigInteger.valueOf(2));
            operations.setLimit(BigInteger.valueOf(1));
            operations.setFirstLink(baseApi + "/families/family2/operations?limit=1&offset=0");
            operations.setPreviousLink(baseApi + "/families/family2/operations?limit=1&offset=1");
            operations.setNextLink(baseApi + "/families/family2/operations?limit=1&offset=3");
            operations.setLastLink(baseApi + "/families/family2/operations?limit=1&offset=3");
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operations;
    }

    public Families mockFamilies(String baseApi, String limit, String offset) {
        Families families = new Families();
        families.setKind(RestExternalConstants.KIND_FAMILIES);
        families.setTotal(BigInteger.valueOf(5));
        if (limit == null && (offset == null || "0".equals(offset))) {
            families.getFamilies().add(mockFamily1Resource());
            families.getFamilies().add(mockFamily2Resource());
            families.getFamilies().add(mockFamily3Resource());
            families.getFamilies().add(mockFamily4Resource());
            families.getFamilies().add(mockFamily15Resource());
            families.setOffset(BigInteger.valueOf(0));
            families.setLimit(BigInteger.valueOf(25));
            families.setFirstLink(null);
            families.setPreviousLink(null);
            families.setNextLink(null);
            families.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            families.getFamilies().add(mockFamily1Resource());
            families.getFamilies().add(mockFamily2Resource());
            families.getFamilies().add(mockFamily3Resource());
            families.getFamilies().add(mockFamily4Resource());
            families.getFamilies().add(mockFamily15Resource());
            families.setOffset(BigInteger.valueOf(0));
            families.setLimit(BigInteger.valueOf(1000));
            families.setFirstLink(null);
            families.setPreviousLink(null);
            families.setNextLink(null);
            families.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            families.getFamilies().add(mockFamily1Resource());
            families.getFamilies().add(mockFamily2Resource());
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(null);
            families.setPreviousLink(null);
            families.setNextLink(baseApi + "/families?limit=2&offset=2");
            families.setLastLink(baseApi + "/families?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            families.getFamilies().add(mockFamily3Resource());
            families.getFamilies().add(mockFamily4Resource());
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(baseApi + "/families?limit=2&offset=0");
            families.setPreviousLink(baseApi + "/families?limit=2&offset=0");
            families.setNextLink(baseApi + "/families?limit=2&offset=4");
            families.setLastLink(baseApi + "/families?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            families.getFamilies().add(mockFamily15Resource());
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(baseApi + "/families?limit=2&offset=0");
            families.setPreviousLink(baseApi + "/families?limit=2&offset=2");
            families.setNextLink(null);
            families.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(baseApi + "/families?limit=2&offset=0");
            families.setPreviousLink(null);
            families.setNextLink(null);
            families.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return families;
    }

    public Families mockFamilies(String baseApi, String limit, String offset, String query) {
        Families families = new Families();
        families.setKind(RestExternalConstants.KIND_FAMILIES);

        String querySupported1 = StatisticalOperationsRestExternalFacadeV10Test.QUERY_FAMILY_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            families.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                families.getFamilies().add(mockFamily1Resource());
                families.getFamilies().add(mockFamily15Resource());
                families.setOffset(BigInteger.valueOf(0));
                families.setLimit(BigInteger.valueOf(25));
                families.setFirstLink(null);
                families.setPreviousLink(null);
                families.setNextLink(null);
                families.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                families.getFamilies().add(mockFamily1Resource());
                families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                families.setFirstLink(null);
                families.setPreviousLink(null);
                families.setNextLink(baseApi + "/families?query=" + query + "&limit=1&offset=1");
                families.setLastLink(baseApi + "/families?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                families.getFamilies().add(mockFamily15Resource());
                families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                families.setFirstLink(baseApi + "/families?query=" + query + "&limit=1&offset=0");
                families.setPreviousLink(baseApi + "/families?query=" + query + "&limit=1&offset=0");
                families.setNextLink(null);
                families.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return families;
    }

    public Families mockFamiliesByOperation1() {
        Families families = new Families();
        families.setKind(RestExternalConstants.KIND_FAMILIES);
        families.setTotal(BigInteger.valueOf(2));
        families.getFamilies().add(mockFamily1Resource());
        families.getFamilies().add(mockFamily2Resource());
        return families;
    }

    public Instances mockInstancesByOperation1(String baseApi, String limit, String offset) {
        Instances instances = new Instances();
        instances.setKind(RestExternalConstants.KIND_INSTANCES);
        instances.setTotal(BigInteger.valueOf(5));
        String operation = StatisticalOperationsRestExternalFacadeV10Test.OPERATION_1;
        if (limit == null && (offset == null || "0".equals(offset))) {
            instances.getInstances().add(mockInstance1Resource());
            instances.getInstances().add(mockInstance2Resource());
            instances.getInstances().add(mockInstance3Resource());
            instances.getInstances().add(mockInstance4Resource());
            instances.getInstances().add(mockInstance15Resource());
            instances.setOffset(BigInteger.valueOf(0));
            instances.setLimit(BigInteger.valueOf(25));
            instances.setFirstLink(null);
            instances.setPreviousLink(null);
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            instances.getInstances().add(mockInstance1Resource());
            instances.getInstances().add(mockInstance2Resource());
            instances.getInstances().add(mockInstance3Resource());
            instances.getInstances().add(mockInstance4Resource());
            instances.getInstances().add(mockInstance15Resource());
            instances.setOffset(BigInteger.valueOf(0));
            instances.setLimit(BigInteger.valueOf(1000));
            instances.setFirstLink(null);
            instances.setPreviousLink(null);
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            instances.getInstances().add(mockInstance1Resource());
            instances.getInstances().add(mockInstance2Resource());
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(null);
            instances.setPreviousLink(null);
            instances.setNextLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=2");
            instances.setLastLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            instances.getInstances().add(mockInstance3Resource());
            instances.getInstances().add(mockInstance4Resource());
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setPreviousLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setNextLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
            instances.setLastLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            instances.getInstances().add(mockInstance15Resource());
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setPreviousLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=2");
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setPreviousLink(null);
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return instances;
    }

    public Instances mockInstancesByOperation1(String baseApi, String limit, String offset, String query) {
        Instances instances = new Instances();
        instances.setKind(RestExternalConstants.KIND_INSTANCES);
        String operation = StatisticalOperationsRestExternalFacadeV10Test.OPERATION_1;

        String querySupported1 = StatisticalOperationsRestExternalFacadeV10Test.QUERY_INSTANCE_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            instances.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                instances.getInstances().add(mockInstance1Resource());
                instances.getInstances().add(mockInstance15Resource());
                instances.setOffset(BigInteger.valueOf(0));
                instances.setLimit(BigInteger.valueOf(25));
                instances.setFirstLink(null);
                instances.setPreviousLink(null);
                instances.setNextLink(null);
                instances.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                instances.getInstances().add(mockInstance1Resource());
                instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                instances.setFirstLink(null);
                instances.setPreviousLink(null);
                instances.setNextLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=1");
                instances.setLastLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                instances.getInstances().add(mockInstance15Resource());
                instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=0");
                instances.setPreviousLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=0");
                instances.setNextLink(null);
                instances.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return instances;
    }

    public SurveyTypes mockSurveyTypes() {
        SurveyTypes surveyTypes = new SurveyTypes();
        surveyTypes.setKind(RestExternalConstants.KIND_SURVEY_TYPES);
        surveyTypes.setTotal(BigInteger.valueOf(2));
        surveyTypes.getSurveyTypes().add(MetamacRestMocks.mockItem("surveyType1"));
        surveyTypes.getSurveyTypes().add(MetamacRestMocks.mockItem("surveyType2"));
        return surveyTypes;
    }

    public OfficialityTypes mockOfficialityTypes() {
        OfficialityTypes officialityTypes = new OfficialityTypes();
        officialityTypes.setKind(RestExternalConstants.KIND_OFFICIALITY_TYPES);
        officialityTypes.setTotal(BigInteger.valueOf(3));
        officialityTypes.getOfficialityTypes().add(MetamacRestMocks.mockItem("officialityType1"));
        officialityTypes.getOfficialityTypes().add(MetamacRestMocks.mockItem("officialityType2"));
        officialityTypes.getOfficialityTypes().add(MetamacRestMocks.mockItem("officialityType3"));
        return officialityTypes;
    }

    public InstanceTypes mockInstanceTypes() {
        InstanceTypes instanceTypes = new InstanceTypes();
        instanceTypes.setKind(RestExternalConstants.KIND_INSTANCE_TYPES);
        instanceTypes.setTotal(BigInteger.valueOf(2));
        instanceTypes.getInstanceTypes().add(MetamacRestMocks.mockItem("instanceType1"));
        instanceTypes.getInstanceTypes().add(MetamacRestMocks.mockItem("instanceType2"));
        return instanceTypes;
    }

    public SurveySources mockSurveySources() {
        SurveySources surveySources = new SurveySources();
        surveySources.setKind(RestExternalConstants.KIND_SURVEY_SOURCES);
        surveySources.setTotal(BigInteger.valueOf(2));
        surveySources.getSurveySources().add(MetamacRestMocks.mockItem("surveySource1"));
        surveySources.getSurveySources().add(MetamacRestMocks.mockItem("surveySource2"));
        return surveySources;
    }

    public CollMethods mockCollMethods() {
        CollMethods collMethods = new CollMethods();
        collMethods.setKind(RestExternalConstants.KIND_COLL_METHODS);
        collMethods.setTotal(BigInteger.valueOf(2));
        collMethods.getCollMethods().add(MetamacRestMocks.mockItem("collMethod1"));
        collMethods.getCollMethods().add(MetamacRestMocks.mockItem("collMethod2"));
        return collMethods;
    }

    public Costs mockCosts() {
        Costs costs = new Costs();
        costs.setKind(RestExternalConstants.KIND_COSTS);
        costs.setTotal(BigInteger.valueOf(2));
        costs.getCosts().add(MetamacRestMocks.mockItem("cost1"));
        costs.getCosts().add(MetamacRestMocks.mockItem("cost2"));
        return costs;
    }

    public Configuration mockExternalApiCommonMetadataRetrieveConfiguration1ById() {
        Configuration configuration = new Configuration();
        configuration.setContact(mockOrganisationResourceFromExternalItemSrm("contact1"));
        configuration.setLegalActs(mockInternationalString("legalActs", "1"));
        configuration.setDataSharing(mockInternationalString("dataSharing", "1"));
        configuration.setConfPolicy(mockInternationalString("confidentialityPolicy", "1"));
        configuration.setConfDataTreatment(mockInternationalString("confidentialityDataTreatment", "1"));
        return configuration;
    }

    private Resource mockOperationResource(String subId) {
        String operationId = "operation" + subId;
        return MetamacRestMocks.mockResource(operationId, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operationId, RestExternalConstants.KIND_OPERATION,
                statisticalOperationsInternalApiBase + "/operations/" + operationId);
    }

    private Resource mockFamilyResource(String subId) {
        String familyId = "family" + subId;
        return MetamacRestMocks.mockResource(familyId, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Family=" + familyId, RestExternalConstants.KIND_FAMILY,
                statisticalOperationsInternalApiBase + "/families/" + familyId);
    }

    private Resource mockInstanceResource(String operationId, String subId) {
        String instanceId = "instance" + subId;
        return MetamacRestMocks.mockResource(instanceId, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Instance=" + operationId + "." + instanceId,
                RestExternalConstants.KIND_INSTANCE, statisticalOperationsInternalApiBase + "/operations/" + operationId + "/instances/" + instanceId);
    }

    private Operation mockOperation(String baseApi, String subId) {

        Operation operation = new Operation();
        operation.setId("operation" + subId);
        operation.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operation.getId());
        operation.setKind(RestExternalConstants.KIND_OPERATION);
        operation.setSelfLink(MetamacRestMocks.mockResourceLink(operation.getKind(), baseApi + "/operations/operation" + subId));
        operation.setTitle(mockInternationalString("operation", subId));
        operation.setAcronym(mockInternationalString("acronym", subId));
        operation.setSubjectArea(mockResourceFromExternalItemSrm("subjectArea1", "subjectAreas", "structuralResources#category"));
        operation.setSecondarySubjectAreas(new SecondarySubjectAreas());
        operation.getSecondarySubjectAreas().setTotal(BigInteger.valueOf(3));
        operation.getSecondarySubjectAreas().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getSecondarySubjectAreas().getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea1", "secundarySubjectAreas", "structuralResources#category"));
        operation.getSecondarySubjectAreas().getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea22", "secundarySubjectAreas", "structuralResources#category"));
        operation.getSecondarySubjectAreas().getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea333", "secundarySubjectAreas", "structuralResources#category"));
        operation.setObjective(mockInternationalString("objetive", subId));
        operation.setDescription(mockInternationalString("description", subId));
        operation.setSurveyType(MetamacRestMocks.mockItem("surveyIdentifier"));
        operation.setOfficialityType(MetamacRestMocks.mockItem("officialityType"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.setProducers(new Producers());
        operation.getProducers().setTotal(BigInteger.valueOf(2));
        operation.getProducers().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getProducers().getProducers().add(mockResourceFromExternalItemSrm("producer1", "producers", "structuralResources#agency"));
        operation.getProducers().getProducers().add(mockResourceFromExternalItemSrm("producer22", "producers", "structuralResources#agency"));
        operation.setRegionalResponsibles(new RegionalResponsibles());
        operation.getRegionalResponsibles().setTotal(BigInteger.valueOf(3));
        operation.getRegionalResponsibles().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getRegionalResponsibles().getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible1", "regionalResponsibles", "structuralResources#agency"));
        operation.getRegionalResponsibles().getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible22", "regionalResponsibles", "structuralResources#agency"));
        operation.getRegionalResponsibles().getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible333", "regionalResponsibles", "structuralResources#agency"));
        operation.setRegionalContributors(new RegionalContributors());
        operation.getRegionalContributors().setTotal(BigInteger.valueOf(2));
        operation.getRegionalContributors().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getRegionalContributors().setTotal(BigInteger.valueOf(2));
        operation.getRegionalContributors().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getRegionalContributors().getRegionalContributors().add(mockResourceFromExternalItemSrm("regionalContributor1", "regionalContributors", "structuralResources#agency"));
        operation.getRegionalContributors().getRegionalContributors().add(mockResourceFromExternalItemSrm("regionalContributor22", "regionalContributors", "structuralResources#agency"));
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(Status.DESIGN);
        operation.setPublishers(new Publishers());
        operation.getPublishers().setTotal(BigInteger.valueOf(3));
        operation.getPublishers().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getPublishers().getPublishers().add(mockResourceFromExternalItemSrm("publisher1", "publishers", "structuralResources#agency"));
        operation.getPublishers().getPublishers().add(mockResourceFromExternalItemSrm("publisher22", "publishers", "structuralResources#agency"));
        operation.getPublishers().getPublishers().add(mockResourceFromExternalItemSrm("publisher333", "publishers", "structuralResources#agency"));
        operation.setRelPolUsAc(mockInternationalString("relPolUsAc", subId));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.setUpdateFrequencies(new UpdateFrequencies());
        operation.getUpdateFrequencies().setTotal(BigInteger.valueOf(4));
        operation.getUpdateFrequencies().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency1", "updateFrequencies", "structuralResources#code"));
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency22", "updateFrequencies", "structuralResources#code"));
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency333", "updateFrequencies", "structuralResources#code"));
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency4444", "updateFrequencies", "structuralResources#code"));
        operation.setCurrentInstance(mockInstanceResource(operation.getId(), "333"));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(mockInternationalString("revPolicy", subId));
        operation.setRevPractice(mockInternationalString("revPractice", subId));
        operation.setContact(mockOrganisationResourceFromExternalItemSrm("contact1"));
        operation.setLegalActs(mockInternationalString("legalActs", "1"));
        operation.setDataSharing(mockInternationalString("dataSharing", "1"));
        operation.setConfidentialityPolicy(mockInternationalString("confidentialityPolicy", "1"));
        operation.setConfidentialityDataTreatment(mockInternationalString("confidentialityDataTreatment", "1"));
        operation.setComment(mockInternationalString("comment", subId));
        operation.setParentLink(MetamacRestMocks.mockResourceLink(RestExternalConstants.KIND_OPERATIONS, baseApi + "/operations"));
        operation.setChildLinks(new ChildLinks());
        operation.getChildLinks().setTotal(BigInteger.valueOf(2));
        operation.getChildLinks().getChildLinks().add(MetamacRestMocks.mockResourceLink(RestExternalConstants.KIND_FAMILIES, baseApi + "/operations/operation" + subId + "/families"));
        operation.getChildLinks().getChildLinks().add(MetamacRestMocks.mockResourceLink(RestExternalConstants.KIND_INSTANCES, baseApi + "/operations/operation" + subId + "/instances"));
        return operation;
    }

    private Family mockFamily(String baseApi, String subId) {

        Family family = new Family();
        family.setId("family" + subId);
        family.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Family=" + family.getId());
        family.setKind(RestExternalConstants.KIND_FAMILY);
        family.setSelfLink(MetamacRestMocks.mockResourceLink(family.getKind(), baseApi + "/families/family" + subId));
        family.setTitle(mockInternationalString("family", subId));
        family.setAcronym(mockInternationalString("acronym", subId));
        family.setDescription(mockInternationalString("description", subId));
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        family.setParentLink(MetamacRestMocks.mockResourceLink(RestExternalConstants.KIND_FAMILIES, baseApi + "/families"));
        family.setChildLinks(new ChildLinks());
        family.getChildLinks().setTotal(BigInteger.valueOf(1));
        family.getChildLinks().getChildLinks().add(MetamacRestMocks.mockResourceLink(RestExternalConstants.KIND_OPERATIONS, baseApi + "/families/family" + subId + "/operations"));
        return family;
    }

    private Instance mockInstance(String baseApi, String subId, String operation) {

        Instance instance = new Instance();
        instance.setId("instance" + subId);
        instance.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Instance=" + operation + "." + instance.getId());
        instance.setKind(RestExternalConstants.KIND_INSTANCE);
        instance.setSelfLink(MetamacRestMocks.mockResourceLink(instance.getKind(), baseApi + "/operations/" + operation + "/instances/instance" + subId));
        instance.setTitle(mockInternationalString("instance", subId));
        instance.setAcronym(mockInternationalString("acronym", subId));
        instance.setSurvey(MetamacRestMocks.mockResource(operation, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operation, RestExternalConstants.KIND_OPERATION,
                baseApi + "/operations/" + operation));
        instance.setPredecessor(mockInstanceResource(operation, "333"));
        instance.setSuccessor(mockInstanceResource(operation, "22"));
        instance.setDataDescription(mockInternationalString("dataDescription", subId));
        instance.setStatisticalPopulation(mockInternationalString("statisticalPopulation", subId));
        instance.setStatisticalUnits(new StatisticalUnits());
        instance.getStatisticalUnits().setTotal(BigInteger.valueOf(2));
        instance.getStatisticalUnits().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getStatisticalUnits().getStatisticalUnits().add(mockResourceFromExternalItemSrm("statisticalUnit1", "statisticalUnits", "structuralResources#concept"));
        instance.getStatisticalUnits().getStatisticalUnits().add(mockResourceFromExternalItemSrm("statisticalUnit22", "statisticalUnits", "structuralResources#concept"));
        // instance.setGeographicGranularity(mockResourceFromExternalItemSrm("geographicGranularity", "geographicGranularities", "structuralResources#codelist")); // TODO metamac-1629
        instance.setGeographicComparability(mockInternationalString("geographicComparability", subId));
        // instance.setTemporalGranularity(mockResourceFromExternalItemSrm("temporalGranularity", "temporalGranularities", "structuralResources#codelist")); // TODO metamac-1629
        instance.setTemporalComparability(mockInternationalString("temporalComparability", subId));
        instance.setBasePeriod("2012");
        instance.setMeasures(new Measures());
        instance.getMeasures().setTotal(BigInteger.valueOf(1));
        instance.getMeasures().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getMeasures().getMeasures().add(mockResourceFromExternalItemSrm("measure1", "measures", "structuralResources#concept"));
        instance.setStatConcDefsDescription(mockInternationalString("statConcDef", subId));
        instance.setStatConcDefs(new StatConcDefs());
        instance.getStatConcDefs().setTotal(BigInteger.valueOf(3));
        instance.getStatConcDefs().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getStatConcDefs().getStatConcDefs().add(mockResourceFromExternalItemSrm("statConcDefList1", "statConcDefLists", "structuralResources#codelist"));
        instance.getStatConcDefs().getStatConcDefs().add(mockResourceFromExternalItemSrm("statConcDefList22", "statConcDefLists", "structuralResources#codelist"));
        instance.getStatConcDefs().getStatConcDefs().add(mockResourceFromExternalItemSrm("statConcDefList333", "statConcDefLists", "structuralResources#codelist"));
        instance.setClassSystemsDescription(mockInternationalString("classSystem", subId));
        instance.setClassSystems(new ClassSystems());
        instance.getClassSystems().setTotal(BigInteger.valueOf(2));
        instance.getClassSystems().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getClassSystems().getClassSystems().add(mockResourceFromExternalItemSrm("classSystemList1", "classSystemLists", "structuralResources#codelist"));
        instance.getClassSystems().getClassSystems().add(mockResourceFromExternalItemSrm("classSystemList22", "classSystemLists", "structuralResources#codelist"));
        instance.setDocMethod(mockInternationalString("docMethod", subId));
        instance.setSurveySource(MetamacRestMocks.mockItem("surveySource1"));
        instance.setCollMethod(MetamacRestMocks.mockItem("collMethod1"));
        instance.setInformationSuppliers(new InformationSuppliers());
        instance.getInformationSuppliers().setTotal(BigInteger.valueOf(1));
        instance.getInformationSuppliers().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getInformationSuppliers().getInformationSuppliers().add(mockResourceFromExternalItemSrm("informationSupplier1", "informationSuppliers", "structuralResources#concept"));
        instance.setFreqColls(new FreqColls());
        instance.getFreqColls().setTotal(BigInteger.valueOf(2));
        instance.getFreqColls().setKind(Do2RestExternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getFreqColls().getFreqColls().add(mockResourceFromExternalItemSrm("freqColl1", "freqColls", "structuralResources#categoryScheme"));
        instance.getFreqColls().getFreqColls().add(mockResourceFromExternalItemSrm("freqColl22", "freqColls", "structuralResources#categoryScheme"));
        instance.setDataValidation(mockInternationalString("dataValidation", subId));
        instance.setDataCompilation(mockInternationalString("dataCompilation", subId));
        instance.setAdjustment(mockInternationalString("adjustment", subId));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        instance.setQualityDoc(mockInternationalString("qualityDoc", subId));
        instance.setQualityAssure(mockInternationalString("qualityAssure", subId));
        instance.setQualityAssmnt(mockInternationalString("qualityAssmnt", subId));
        instance.setUserNeeds(mockInternationalString("userNeeds", subId));
        instance.setUserSat(mockInternationalString("userSat", subId));
        instance.setCompleteness(mockInternationalString("completeness", subId));
        instance.setTimeliness(mockInternationalString("timeliness", subId));
        instance.setPunctuality(mockInternationalString("punctuality", subId));
        instance.setAccuracyOverall(mockInternationalString("accuracyOverall", subId));
        instance.setSamplingErr(mockInternationalString("samplingErr", subId));
        instance.setNonsamplingErr(mockInternationalString("nonsamplingErr", subId));
        instance.setCoherXDom(mockInternationalString("coherXDom", subId));
        instance.setCoherInternal(mockInternationalString("coherInternal", subId));
        instance.setComment(mockInternationalString("comment", subId));
        instance.setParentLink(MetamacRestMocks.mockResourceLink(RestExternalConstants.KIND_OPERATION, baseApi + "/operations/" + operation));
        // no children
        return instance;
    }

    private Resource mockOperation1Resource() {
        return mockOperationResource("1");
    }

    private Resource mockOperation2Resource() {
        return mockOperationResource("2");
    }

    private Resource mockOperation3Resource() {
        return mockOperationResource("3");
    }

    private Resource mockOperation4Resource() {
        return mockOperationResource("4");
    }

    private Resource mockOperation5Resource() {
        return mockOperationResource("5");
    }

    private Resource mockOperation6Resource() {
        return mockOperationResource("6");
    }

    private Resource mockOperation7Resource() {
        return mockOperationResource("7");
    }

    private Resource mockOperation8Resource() {
        return mockOperationResource("8");
    }

    private Resource mockOperation9Resource() {
        return mockOperationResource("9");
    }

    private Resource mockOperation10Resource() {
        return mockOperationResource("10");
    }

    private Resource mockFamily1Resource() {
        return mockFamilyResource("1");
    }

    private Resource mockFamily2Resource() {
        return mockFamilyResource("2");
    }

    private Resource mockFamily3Resource() {
        return mockFamilyResource("3");
    }

    private Resource mockFamily4Resource() {
        return mockFamilyResource("4");
    }

    private Resource mockFamily15Resource() {
        return mockFamilyResource("15");
    }

    private Resource mockInstance1Resource() {
        return mockInstanceResource(StatisticalOperationsRestExternalFacadeV10Test.OPERATION_1, "1");
    }

    private Resource mockInstance2Resource() {
        return mockInstanceResource(StatisticalOperationsRestExternalFacadeV10Test.OPERATION_1, "2");
    }

    private Resource mockInstance3Resource() {
        return mockInstanceResource(StatisticalOperationsRestExternalFacadeV10Test.OPERATION_1, "3");
    }

    private Resource mockInstance4Resource() {
        return mockInstanceResource(StatisticalOperationsRestExternalFacadeV10Test.OPERATION_1, "4");
    }

    private Resource mockInstance15Resource() {
        return mockInstanceResource(StatisticalOperationsRestExternalFacadeV10Test.OPERATION_1, "15");
    }

    private InternationalString mockInternationalString(String metadata, String subsubTitle) {
        String subTitle = subsubTitle != null ? metadata + subsubTitle : metadata;
        return MetamacRestMocks.mockInternationalString("es", subTitle + " en Español", "en", subTitle + " in English");
    }

    private Resource mockResourceFromExternalItemSrm(String id, String apiSubpath, String kind) {
        return mockResourceFromExternalItem(id, srmInternalApiBase + "/v1.0", apiSubpath, kind);
    }

    private Resource mockResourceFromExternalItem(String id, String endpointApi, String apiSubpath, String kind) {
        String urn = "urn:" + id;
        String selfLink = endpointApi + "/" + apiSubpath + "/" + id;
        return MetamacRestMocks.mockResource(id, urn, kind, selfLink);
    }

    private ResourceInternal mockOrganisationResourceFromExternalItemSrm(String id) {
        ResourceInternal resource = new ResourceInternal();
        resource.setId(id);
        resource.setUrn("urn:sdmx:org.sdmx.infomodel.base.Agency=SDMX:AGENCIES(1.0)." + id);
        resource.setKind("structuralResources#agency");
        resource.setSelfLink(MetamacRestMocks.mockResourceLink(resource.getKind(), srmInternalApiBase + "/v1.0/agencyschemes/SDMX/AGENCIES/1.0/agencies/" + id));
        resource.setTitle(MetamacRestMocks.mockInternationalString("es", id + " en Español", "en", id + " in English"));
        return resource;
    }
}