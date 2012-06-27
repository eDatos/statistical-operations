package org.siemac.metamac.statistical.operations.core.serviceapi;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(Suite.class)
@Suite.SuiteClasses({SecurityStatisticalOperationsServiceFacadeTest.class, StatisticalOperationsBaseServiceTest.class, StatisticalOperationsListsServiceTest.class,
        StatisticalOperationsServiceFacadeTest.class})
public class StatisticalOperationsSuite {
}
